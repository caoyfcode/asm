#[macro_use]
mod elf_h; // 根据 elf.h 编写

/// ELF File
///
/// +----------------------+
/// |      ELF Header      |
/// +----------------------+
/// | program header table | (Executable only)
/// +----------------------+ padding when Executable
/// |        .text         |
/// +----------------------+ padding when Executable
/// |        .data         |
/// +----------------------+
/// |        .bss          | 大小为 0
/// +----------------------+
/// |      .shstrtab       |
/// +----------------------+ padding
/// | section header table |
/// +----------------------+
/// |       .symtab        |
/// +----------------------+
/// |       .strtab        |
/// +----------------------+ padding
/// |      .rel.text       | (Relocatable only)
/// +----------------------+
/// |      .rel.data       | (Relocatable only)
/// +----------------------+

/// section header table
///
/// +----------------------+
/// |        UNDEF         | 0
/// +----------------------+
/// |        .text         | 1
/// +----------------------+
/// |        .data         | 2
/// +----------------------+
/// |        .bss          | 3
/// +----------------------+
/// |      .shstrtab       | 4
/// +----------------------+
/// |       .symtab        | 5
/// +----------------------+
/// |       .strtab        | 6
/// +----------------------+
/// |      .rel.text       | 7 (Relocatable only)
/// +----------------------+
/// |      .rel.data       | 8 (Relocatable only)
/// +----------------------+

/// program header table
///
/// +-----------------------------------+
/// |  ELF header, Program Header Table | 0
/// +-----------------------------------+
/// |             .text                 | 1
/// +-----------------------------------+
/// |           .data, .bss             | 2
/// +-----------------------------------+

use std::{collections::HashMap, fs::File, mem::size_of, io::Write};

pub use elf_h::*;


/// 表示目标文件或可执行文件
pub struct Elf {
    text: Vec<u8>, // .text
    data: Vec<u8>, // .data
    bss_size: Word, // .bss
    symbol_table: (Vec<Sym>, Vec<Sym>), // .symtab (locals, globals)
    string_table: StringTable, // .strtab
    text_rel: Vec<Rel>, // .rel.text
    data_rel: Vec<Rel>, // .rel.data

    symbol_index: Option<HashMap<usize, usize>>, // name -> index, name 指的是 string_table 的 index, index 指的是 symbol table 的 idnex
}

impl Elf {
    // 各个 section 在 section header table 中的下标
    const TEXT_INDEX: Section = 1;
    const DATA_INDEX: Section = 2;
    const BSS_INDEX: Section = 3;
    const SHSTRTAB_INDEX: Section = 4;
    const SYMTAB_INDEX: Section = 5;
    const STRTAB_INDEX: Section = 6;
    // 当为可执行文件时, base address
    const BASE_ADDRESS: u32 = 0x8048000;

    pub fn new() -> Self {
        let mut obj = Self {
            text: Vec::new(),
            data: Vec::new(),
            bss_size: 0,
            symbol_table: (Vec::new(), Vec::new()),
            string_table: StringTable::new(),
            text_rel: Vec::new(),
            data_rel: Vec::new(),
            symbol_index: None,
        };
        // 符号表先添加一个无效符号
        let index = obj.string_table.add(String::from(""));
        obj.symbol_table.0.push(Sym {
            st_name: index as Word,
            st_value: 0,
            st_size: 0,
            st_info: st_info!(STB_LOCAL, STT_NOTYPE),
            st_other: 0,
            st_shndx: 0, // undef
        });
        obj
    }

    pub fn set_bss_size(&mut self, size: u32) {
        self.bss_size = size;
    }

    /// 设置 .text 或 .data 的内容
    pub fn set_section_content(&mut self, sec_name: &str, content: Vec<u8>) -> Option<()>{
        match sec_name {
            ".text" => self.text = content,
            ".data" => self.data = content,
            _ => return None,
        }
        Some(())
    }

    /// 在符号表添加一个符号, 必须是 .text 或 .data 或者 .bss 或者 undef 的
    pub fn add_symbol(&mut self, name: String, is_undef: bool, value: u32, sec_name: &str, is_global: bool) -> Option<()> {
        if let Some(_) = self.symbol_index { // 不可再添加符号, 因为已经固定
            return None;
        }
        if let Some(_) = self.string_table.get(&name) {
            return None;
        }
        let index = self.string_table.add(name);
        let mut sym = Sym {
            st_name: index as Word,
            st_value: 0,
            st_size: 0,
            st_info: 0,
            st_other: 0,
            st_shndx: 0,
        };
        if is_undef {
            sym.st_info = st_info!(STB_GLOBAL, STT_NOTYPE);
            sym.st_shndx = 0; // undef
            self.symbol_table.1.push(sym);
        } else {
            sym.st_shndx = match sec_name {
                ".text" => Self::TEXT_INDEX,
                ".data" => Self::DATA_INDEX,
                ".bss" => Self::BSS_INDEX,
                _ => return None,
            };
            sym.st_value = value;
            if is_global {
                sym.st_info = st_info!(STB_GLOBAL, STT_NOTYPE);
                self.symbol_table.1.push(sym);
            } else {
                sym.st_info = st_info!(STB_LOCAL, STT_NOTYPE);
                self.symbol_table.0.push(sym)
            }
        }
        Some(())
    }

    /// 在重定位表添加一个表项(调用时必须保证不再调用 add_symbol), 对于可执行文件, 最终写入文件时会对所有重定位项进行处理
    pub fn add_relocation(&mut self, offset: u32, section: &str, symbol: &String, is_relative: bool) -> Option<()> {
        if self.symbol_index.is_none() {
            self.build_symbol_index();
        }
        let symbol_name = self.string_table.get(&symbol)?;
        let symbol = self.symbol_index.as_ref().unwrap().get(&symbol_name)?.clone();
        let r_info: Word = if is_relative {
            r_info!(symbol, R_386_PC32)
        } else {
            r_info!(symbol, R_386_32)
        };
        let rel = Rel {
            r_offset: offset,
            r_info,
        };
        match section {
            ".text" => self.text_rel.push(rel),
            ".data" => self.data_rel.push(rel),
            _ => return None,
        }
        Some(())
    }

    /// 构造 symbol name 到 symbol table index 的映射, 之后符号表就固定了
    /// 1. 在调用此之前, 不可添加重定位符号
    /// 2. 之后, 不可添加符号表
    fn build_symbol_index(&mut self) {
        let mut index: usize = 0;
        let mut map: HashMap<usize, usize> = HashMap::new();
        for local in &self.symbol_table.0 {
            map.insert(local.st_name as usize, index);
            index += 1;
        }
        for global in &self.symbol_table.1 {
            map.insert(global.st_name as usize, index);
            index += 1;
        }
        self.symbol_index = Some(map);
    }

    pub fn write_obj(&self, out: &mut File) {
        // .shstrtab
        let shstrtab = StringTable::from(&[
            "", ".bss", ".shstrtab", ".symtab", ".strtab",
            ".rel.text", ".rel.data", // .text 与 .data 也包含其中
        ]);

        // offsets
        let text_off = size_of::<Ehdr>(); // align = 1
        let data_off = text_off + self.text.len(); // align = 1
        let bss_off = data_off + self.data.len(); // align = 1
        let shstrtab_off = bss_off; // align = 1
        let shstrtab_end = shstrtab_off + shstrtab.size();
        let sh_off = align(shstrtab_end, 4); // align = 4;
        let sh_padding = sh_off - shstrtab_end;
        let symtab_off = sh_off + 9 * size_of::<Shdr>(); // align = 4;
        let strtab_off = symtab_off +
            (self.symbol_table.0.len() +
            self.symbol_table.1.len()) * size_of::<Sym>(); // align = 1
        let strtab_end = strtab_off + self.string_table.size();
        let rel_text_off = align(strtab_end, 4); // align = 4
        let rel_text_padding = rel_text_off - strtab_end;
        let rel_data_off = rel_text_off + self.text_rel.len() * size_of::<Rel>(); // align = 4

        // section header table
        let section_header_table: [Shdr; 9] = [
            Shdr { // undef
                sh_name: shstrtab.get(&String::from("")).unwrap() as Word,
                sh_type: SHT_NULL,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: 0,
                sh_size: 0,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 0,
                sh_entsize: 0,
            },
            Shdr { // .text
                sh_name: shstrtab.get(&String::from(".rel.text")).unwrap() as Word + 4,
                sh_type: SHT_PROGBITS,
                sh_flags: SHF_ALLOC | SHF_EXECINSTR,
                sh_addr: 0,
                sh_offset: text_off as Off,
                sh_size: self.text.len() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .data
                sh_name: shstrtab.get(&String::from(".rel.data")).unwrap() as Word + 4,
                sh_type: SHT_PROGBITS,
                sh_flags: SHF_ALLOC | SHF_WRITE,
                sh_addr: 0,
                sh_offset: data_off as Off,
                sh_size: self.data.len() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .bss
                sh_name: shstrtab.get(&String::from(".bss")).unwrap() as Word,
                sh_type: SHT_NOBITS,
                sh_flags: SHF_ALLOC | SHF_WRITE,
                sh_addr: 0,
                sh_offset: bss_off as Off,
                sh_size: self.bss_size,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .shstrtab
                sh_name: shstrtab.get(&String::from(".shstrtab")).unwrap() as Word,
                sh_type: SHT_STRTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: shstrtab_off as Off,
                sh_size: shstrtab.size() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .symtab
                sh_name: shstrtab.get(&String::from(".symtab")).unwrap() as Word,
                sh_type: SHT_SYMTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: symtab_off as Off,
                sh_size: ((self.symbol_table.0.len() +
                    self.symbol_table.1.len()) * size_of::<Sym>()) as Word,
                sh_link: Self::STRTAB_INDEX as Word, // string table
                sh_info: self.symbol_table.0.len() as Word, // last local + 1
                sh_addralign: 4,
                sh_entsize: size_of::<Sym>() as Word,
            },
            Shdr { // .strtab
                sh_name: shstrtab.get(&String::from(".strtab")).unwrap() as Word,
                sh_type: SHT_STRTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: strtab_off as Off,
                sh_size: self.string_table.size() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .rel.text
                sh_name: shstrtab.get(&String::from(".rel.text")).unwrap() as Word,
                sh_type: SHT_REL,
                sh_flags: SHF_INFO_LINK,
                sh_addr: 0,
                sh_offset: rel_text_off as Off,
                sh_size: (self.text_rel.len() * size_of::<Rel>()) as Word,
                sh_link: Self::SYMTAB_INDEX as Word, // symbol table
                sh_info: Self::TEXT_INDEX as Word, // .text
                sh_addralign: 4,
                sh_entsize: size_of::<Rel>() as Word,
            },
            Shdr { // .rel.data
                sh_name: shstrtab.get(&String::from(".rel.data")).unwrap() as Word,
                sh_type: SHT_REL,
                sh_flags: SHF_INFO_LINK,
                sh_addr: 0,
                sh_offset: rel_data_off as Off,
                sh_size: (self.data_rel.len() * size_of::<Rel>()) as Word,
                sh_link: Self::SYMTAB_INDEX as Word, // symbol table
                sh_info: Self::DATA_INDEX as Word, // .data
                sh_addralign: 4,
                sh_entsize: size_of::<Rel>() as Word,
            },
        ];

        // write to file
        let header = Ehdr {
            e_ident: [
                ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
                ELFCLASS32, ELFDATA2LSB, EV_CURRENT as u8,
                ELFOSABI_NONE, 0,
                0, 0, 0, 0, 0, 0, 0
                ],
            e_type: ET_REL,
            e_machine: EM_386,
            e_version: EV_CURRENT,
            e_entry: 0,
            e_phoff: 0,
            e_shoff: sh_off as Off,
            e_flags: 0,
            e_ehsize: size_of::<Ehdr>() as Half,
            e_phentsize: size_of::<Phdr>() as Half,
            e_phnum: 0,
            e_shentsize: size_of::<Shdr>() as Half,
            e_shnum: 9,
            e_shstrndx: Self::SHSTRTAB_INDEX,
        };
        // ELF header
        out.write_all(unsafe { serialize(&header) }).unwrap();
        // .text section
        out.write_all(&self.text).unwrap();
        // .data section
        out.write_all(&self.data).unwrap();
        // .shstrtab
        out.write_all(shstrtab.content()).unwrap();
        for _ in 0..sh_padding {
            out.write_all(&[0]).unwrap();
        }
        // section header table
        out.write_all(unsafe { serialize_slice(&section_header_table) }).unwrap();
        // .symtab
        out.write_all(unsafe { serialize_slice(&self.symbol_table.0) }).unwrap(); // local
        out.write_all(unsafe { serialize_slice(&self.symbol_table.1) }).unwrap(); // global
        // .strtab
        out.write_all(self.string_table.content()).unwrap();
        for _ in 0..rel_text_padding {
            out.write_all(&[0]).unwrap();
        }
        // .rel.text
        out.write_all(unsafe { serialize_slice(&self.text_rel) }).unwrap();
        // .rel.data
        out.write_all(unsafe { serialize_slice(&self.data_rel) }).unwrap();
    }

    pub fn write_exec(&mut self, out: &mut File) {
        // .shstrtab
        let shstrtab = StringTable::from(&[
            "", ".text", ".data", ".bss",
            ".shstrtab", ".symtab", ".strtab",
        ]);

        // offsets
        let ph_off = size_of::<Ehdr>();
        let header_end = size_of::<Ehdr>() + 3 * size_of::<Phdr>();
        let text_off = align(header_end, 0x1000); // align = 0x1000
        let text_padding = text_off - header_end;
        let text_addr = Self::BASE_ADDRESS + text_off as u32;
        let text_end = text_off + self.text.len();
        let data_off = align(text_end, 0x1000); // align = 0x1000
        let data_padding = data_off - text_end;
        let data_addr = Self::BASE_ADDRESS + data_off as Addr;
        let bss_off = data_off + self.data.len(); // align = 1
        let bss_addr = data_addr + self.data.len() as u32;
        let shstrtab_off = bss_off; // align = 1
        let shstrtab_end = shstrtab_off + shstrtab.size();
        let sh_off = align(shstrtab_end, 4); // align = 4;
        let sh_padding = sh_off - shstrtab_end;
        let symtab_off = sh_off + 7 * size_of::<Shdr>(); // align = 4;
        let strtab_off = symtab_off +
            (self.symbol_table.0.len() +
            self.symbol_table.1.len()) * size_of::<Sym>(); // align = 1
        // entry
        let entry = self.string_table.get(&String::from("_start")).expect("can't find \"_start\"");
        if self.symbol_index.is_none() {
            self.build_symbol_index();
        }
        let entry = self.symbol_index.as_ref().unwrap().get(&entry).unwrap().clone();
        let entry = if entry < self.symbol_table.0.len() {
            self.symbol_table.0[entry]
        } else {
            self.symbol_table.1[entry - self.symbol_table.0.len()]
        };
        if entry.st_shndx != Self::TEXT_INDEX {
            panic!("\"_start\" is not in .text");
        };
        let entry = text_addr + entry.st_value;

        // program header table
        let program_header_table: [Phdr; 3] = [
            Phdr { // headers
                p_type: PT_LOAD,
                p_offset: 0,
                p_vaddr: Self::BASE_ADDRESS,
                p_paddr: Self::BASE_ADDRESS,
                p_filesz: header_end as Word,
                p_memsz: header_end as Word,
                p_flags: PF_R,
                p_align: 0x1000,
            },
            Phdr {
                p_type: PT_LOAD,
                p_offset: text_off as Off,
                p_vaddr: text_addr,
                p_paddr: text_addr,
                p_filesz: self.text.len() as Word,
                p_memsz: self.text.len() as Word,
                p_flags: PF_R | PF_X,
                p_align: 0x1000,
            },
            Phdr {
                p_type: PT_LOAD,
                p_offset: data_off as Off,
                p_vaddr: data_addr,
                p_paddr: data_addr,
                p_filesz: self.data.len() as Word,
                p_memsz: self.data.len() as Word + self.bss_size as Word,
                p_flags: PF_R | PF_W,
                p_align: 0x1000,
            },
        ];

        // section header table
        let section_header_table: [Shdr; 7] = [
            Shdr { // undef
                sh_name: shstrtab.get(&String::from("")).unwrap() as Word,
                sh_type: SHT_NULL,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: 0,
                sh_size: 0,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 0,
                sh_entsize: 0,
            },
            Shdr { // .text
                sh_name: shstrtab.get(&String::from(".text")).unwrap() as Word,
                sh_type: SHT_PROGBITS,
                sh_flags: SHF_ALLOC | SHF_EXECINSTR,
                sh_addr: 0,
                sh_offset: text_off as Off,
                sh_size: self.text.len() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .data
                sh_name: shstrtab.get(&String::from(".data")).unwrap() as Word,
                sh_type: SHT_PROGBITS,
                sh_flags: SHF_ALLOC | SHF_WRITE,
                sh_addr: 0,
                sh_offset: data_off as Off,
                sh_size: self.data.len() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .bss
                sh_name: shstrtab.get(&String::from(".bss")).unwrap() as Word,
                sh_type: SHT_NOBITS,
                sh_flags: SHF_ALLOC | SHF_WRITE,
                sh_addr: 0,
                sh_offset: bss_off as Off,
                sh_size: self.bss_size,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .shstrtab
                sh_name: shstrtab.get(&String::from(".shstrtab")).unwrap() as Word,
                sh_type: SHT_STRTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: shstrtab_off as Off,
                sh_size: shstrtab.size() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .symtab
                sh_name: shstrtab.get(&String::from(".symtab")).unwrap() as Word,
                sh_type: SHT_SYMTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: symtab_off as Off,
                sh_size: ((self.symbol_table.0.len() +
                    self.symbol_table.1.len()) * size_of::<Sym>()) as Word,
                sh_link: Self::STRTAB_INDEX as Word, // string table
                sh_info: self.symbol_table.0.len() as Word, // last local + 1
                sh_addralign: 4,
                sh_entsize: size_of::<Sym>() as Word,
            },
            Shdr { // .strtab
                sh_name: shstrtab.get(&String::from(".strtab")).unwrap() as Word,
                sh_type: SHT_STRTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: strtab_off as Off,
                sh_size: self.string_table.size() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
        ];
        self.handle_relocations(".text", text_addr, data_addr, bss_addr).unwrap();
        self.handle_relocations(".data", text_addr, data_addr, bss_addr).unwrap();
        // write to file
        let header = Ehdr {
            e_ident: [
                ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
                ELFCLASS32, ELFDATA2LSB, EV_CURRENT as u8,
                ELFOSABI_NONE, 0,
                0, 0, 0, 0, 0, 0, 0
                ],
            e_type: ET_EXEC,
            e_machine: EM_386,
            e_version: EV_CURRENT,
            e_entry: entry,
            e_phoff: ph_off as Off,
            e_shoff: sh_off as Off,
            e_flags: 0,
            e_ehsize: size_of::<Ehdr>() as Half,
            e_phentsize: size_of::<Phdr>() as Half,
            e_phnum: 3,
            e_shentsize: size_of::<Shdr>() as Half,
            e_shnum: 7,
            e_shstrndx: Self::SHSTRTAB_INDEX,
        };
        // ELF header
        out.write_all(unsafe { serialize(&header) }).unwrap();
        // program header table
        out.write_all(unsafe { serialize_slice(&program_header_table) }).unwrap();
        for _ in 0..text_padding {
            out.write_all(&[0]).unwrap();
        }
        // .text
        out.write_all(&self.text).unwrap();
        for _ in 0..data_padding {
            out.write_all(&[0x90]).unwrap();
        }
        // .data
        out.write_all(&self.data).unwrap();
        // .shstrtab
        out.write_all(shstrtab.content()).unwrap();
        for _ in 0..sh_padding {
            out.write_all(&[0]).unwrap();
        }
        // section header table
        out.write_all(unsafe { serialize_slice(&section_header_table) }).unwrap();
        // .symtab
        out.write_all(unsafe { serialize_slice(&self.symbol_table.0) }).unwrap(); // local
        out.write_all(unsafe { serialize_slice(&self.symbol_table.1) }).unwrap(); // global
        // .strtab
        out.write_all(self.string_table.content()).unwrap();
    }

    /// 处理重定位
    fn handle_relocations(&mut self, section: &str, text_addr: u32, data_addr: u32, bss_addr: u32) -> Option<()> {
        let (sec, sec_rels, sec_addr) = match section {
            ".text" => (&mut self.text, &self.text_rel, text_addr),
            ".data" => (&mut self.data, &self.data_rel, data_addr),
            _ => return None,
        };
        for rel in sec_rels {
            // 符号的信息
            let symbol = r_sym!(rel.r_info) as usize;
            let symbol = if symbol < self.symbol_table.0.len() {
                self.symbol_table.0[symbol as usize]
            } else {
                self.symbol_table.1[symbol as usize - self.symbol_table.0.len()]
            };
            let symbol_addr = match symbol.st_shndx {
                Self::TEXT_INDEX => text_addr + symbol.st_value,
                Self::DATA_INDEX => data_addr + symbol.st_value,
                Self::BSS_INDEX => bss_addr + symbol.st_value,
                _ => panic!("can't be here"),
            };
            if symbol.st_size != 0 {
                return None;
            }
            // 重定位的位置
            let rel_addr = sec_addr + rel.r_offset;
            let index = rel.r_offset as usize;
            if index >= sec.len() || index + 3 >= sec.len() {
                return None;
            }
            // 重定位类型
            let is_relative = match r_type!(rel.r_info) as u8 {
                R_386_32 => false,
                R_386_PC32 => true,
                _ => return None, // unsupported reloaction type
            };
            // 计算重定位的值
            let value: u32 = if is_relative {
                let addend: u32 = sec[index] as u32
                    + ((sec[index + 1] as u32) << 8)
                    + ((sec[index + 2] as u32) << 16)
                    + ((sec[index + 3] as u32) << 24);
                symbol_addr
                    .wrapping_add(addend)
                    .wrapping_sub(rel_addr)
            } else {
                symbol_addr
            };
            // 进行重定位
            sec[index] = value as u8;
            sec[index + 1] = (value >> 8) as u8;
            sec[index + 2] = (value >> 16) as u8;
            sec[index + 3] = (value >> 24) as u8;
        }
        Some(())
    }
}

struct StringTable {
    content: Vec<u8>,
    index_map: HashMap<String, usize>,
}

impl StringTable {
    fn new() -> Self {
        Self {
            content: Vec::new(),
            index_map: HashMap::new(),
        }
    }

    fn from(strs: &[&str]) -> Self {
        let mut ret = Self::new();
        for str in strs {
            ret.add(String::from(*str));
        }
        ret
    }

    fn add(&mut self, str: String) -> usize {
        if let Some(index) = self.index_map.get(&str) {
            return *index;
        }
        let index = self.content.len();
        self.content.extend_from_slice(str.as_bytes());
        self.content.push(0);
        self.index_map.insert(str, index);
        return index;
    }

    /// index of a String
    fn get(&self, str: &String) -> Option<usize> {
        self.index_map.get(str).copied()
    }

    fn content(&self) -> &[u8] {
        &self.content
    }

    fn size(&self) -> usize {
        self.content.len()
    }
}

fn align(addr: usize, align: usize) -> usize {
    addr + (align - addr % align) % align
}

unsafe fn serialize<T: Sized>(src: &T) -> &[u8] {
    std::slice::from_raw_parts(
        (src as *const T) as *const u8,
        size_of::<T>()
    )
}

unsafe fn serialize_slice<T: Sized>(src: &[T]) -> &[u8] {
    std::slice::from_raw_parts(
        src.as_ptr() as *const u8,
        size_of::<T>() * src.len()
    )
}

#[cfg(test)]
mod tests_obj {
    use super::*;

    #[test]
    fn test_simple_exit() {
        let code: Vec<u8> = vec![
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax ; 调用号 1: exit
            0xbb, 0x02, 0x00, 0x00, 0x00, // mov $2, %ebx ; 参数 1: 2
            0xcd, 0x80 // int $0x80
        ];
        let mut obj = Elf::new();
        obj.set_section_content(".text", code).unwrap();
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file);
        // $ ld -melf_i386 -o test test.o
        // $ ./test
        // $ echo $?
        // 2
    }

    #[test]
    fn test_absolute_reloaction_data_label() {
        let data: Vec<u8> = vec![
            0x02,  // .byte 2
            0x05, 0x00, 0x00, 0x00, // num: .int 5
        ];
        let code: Vec<u8> = vec![
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax ; 调用号 1: exit
            0x8b, 0x1d, 0x00, 0x00, 0x00, 0x00, // mov 0, %ebx ; 参数 1: 0, 之后使用重定位改变
            0xcd, 0x80 // int $0x80
        ];
        let mut obj = Elf::new();
        obj.set_section_content(".text", code).unwrap();
        obj.set_section_content(".data", data).unwrap();
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        obj.add_symbol(String::from("num"), false, 1, ".data", false).unwrap();
        obj.add_relocation(7, ".text", &String::from("num"), false).unwrap();

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file);
        // $ ld -melf_i386 -o test test.o
        // $ ./test
        // $ echo $?
        // 5
    }

    #[test]
    fn test_absolute_reloaction_bss_label() {
        let bss_size: u32 = 4;
        let code: Vec<u8> = vec![
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax ; 调用号 1: exit
            0x8b, 0x1d, 0x00, 0x00, 0x00, 0x00, // mov 0, %ebx ; 参数 1: 0, 之后使用重定位改变
            0xcd, 0x80 // int $0x80
        ];
        let mut obj = Elf::new();
        obj.set_section_content(".text", code).unwrap();
        obj.set_bss_size(bss_size);
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        obj.add_symbol(String::from("num"), false, 0, ".bss", false).unwrap();
        obj.add_relocation(7, ".text", &String::from("num"), false).unwrap();

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file);
        // $ ld -melf_i386 -o test test.o
        // $ ./test
        // $ echo $?
        // 0
    }

    #[test]
    fn test_relative_relocation() {
        let code_start: Vec<u8> = vec![
            0x68, 0x03, 0x00, 0x00, 0x00, // pushl $3
            0xe8, 0xfc, 0xff, 0xff, 0xff, // call something
        ];
        let code_exit: Vec<u8> = vec! [
            0x8b, 0x5c, 0x24, 0x04, // mov 4(%esp), %ebx
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax
            0xcd, 0x80 // int $0x80
        ];
        let mut code: Vec<u8> = code_start.clone();
        code.extend(code_exit.iter());

        let mut obj = Elf::new();
        obj.set_section_content(".text", code).unwrap();
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        obj.add_symbol(String::from("exit"), false, code_start.len() as u32, ".text", false).unwrap();
        obj.add_relocation(code_start.len() as u32 - 4, ".text", &String::from("exit"), true).unwrap(); // 添加相对重定位

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file);

        // $ ld -melf_i386 -o test test.o
        // $ ./test
        // $ echo $?
        // 3
    }
}

#[cfg(test)]
mod tests_exec {
    use std::os::unix::prelude::PermissionsExt;

    use super::*;

    #[test]
    fn test_simple_exit() {
        let code: Vec<u8> = vec![
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax ; 调用号 1: exit
            0xbb, 0x02, 0x00, 0x00, 0x00, // mov $2, %ebx ; 参数 1: 2
            0xcd, 0x80 // int $0x80
        ];
        let mut exec = Elf::new();
        exec.set_section_content(".text", code).unwrap();
        exec.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();

        let mut file = File::create("test").expect("fail to create a file");
        let mut perms = file.metadata().unwrap().permissions();
        perms.set_mode(0o755); // rwx r-x r-x
        file.set_permissions(perms).unwrap();
        exec.write_exec(&mut file);
        // $ ./test; echo $?
        // 2
    }

    #[test]
    fn test_absolute_reloaction_data_label() {
        let data: Vec<u8> = vec![
            0x02,  // .byte 2
            0x05, 0x00, 0x00, 0x00, // num: .int 5
        ];
        let code: Vec<u8> = vec![
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax ; 调用号 1: exit
            0x8b, 0x1d, 0x00, 0x00, 0x00, 0x00, // mov 0, %ebx ; 参数 1: 0, 之后使用重定位改变
            0xcd, 0x80 // int $0x80
        ];
        let mut exec = Elf::new();
        exec.set_section_content(".text", code).unwrap();
        exec.set_section_content(".data", data).unwrap();
        exec.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        exec.add_symbol(String::from("num"), false, 1, ".data", false).unwrap();
        exec.add_relocation(7, ".text", &String::from("num"), false).unwrap();

        let mut file = File::create("test").expect("fail to create a file");
        let mut perms = file.metadata().unwrap().permissions();
        perms.set_mode(0o755); // rwx r-x r-x
        file.set_permissions(perms).unwrap();
        exec.write_exec(&mut file);
        // $ ./test; echo $?
        // 5
    }

    #[test]
    fn test_absolute_reloaction_bss_label() {
        let bss_size: u32 = 4;
        let code: Vec<u8> = vec![
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax ; 调用号 1: exit
            0x8b, 0x1d, 0x00, 0x00, 0x00, 0x00, // mov 0, %ebx ; 参数 1: 0, 之后使用重定位改变
            0xcd, 0x80 // int $0x80
        ];
        let mut exec = Elf::new();
        exec.set_section_content(".text", code).unwrap();
        exec.set_bss_size(bss_size);
        exec.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        exec.add_symbol(String::from("num"), false, 0, ".bss", false).unwrap();
        exec.add_relocation(7, ".text", &String::from("num"), false).unwrap();

        let mut file = File::create("test").expect("fail to create a file");
        let mut perms = file.metadata().unwrap().permissions();
        perms.set_mode(0o755); // rwx r-x r-x
        file.set_permissions(perms).unwrap();
        exec.write_exec(&mut file);
        // $ ./test; echo $?
        // 0
    }
}
