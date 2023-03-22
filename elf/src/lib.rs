#[macro_use]
pub mod elf_h; // 根据 elf.h 编写

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

use elf_h::*;

#[derive(Debug)]
pub enum ElfError {
    DuplicateSymbol(String), // symbol name
    MissingSymbol(String), // symbal name
    WriteFileError(std::io::Error), // error message
    ExecutableRelocation, // executable file should not have relocation sections
}

impl std::fmt::Display for ElfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ElfError::DuplicateSymbol(symbol) => {
                write!(f, "duplicate symbol \"{}\"", symbol)
            },
            ElfError::MissingSymbol(symbol) => {
                write!(f, "missing symbol \"{}\"", symbol)
            },
            ElfError::WriteFileError(cause) => {
                write!(f, "{}", cause)
            },
            ElfError::ExecutableRelocation => {
                write!(f, "relocation section in executable file")
            }
        }
    }
}

impl std::error::Error for ElfError {}

impl From<std::io::Error> for ElfError {
    fn from(value: std::io::Error) -> Self {
        Self::WriteFileError(value)
    }
}

pub struct ElfWriter {
    text: Vec<u8>,
    data: Vec<u8>,
    bss_size: u32,
    symbols: Vec<Symbol>,
    rel_text: Vec<Relocation>,
    rel_data: Vec<Relocation>,
}

impl ElfWriter {
    // 各个 section 在 section header table 中的下标
    const TEXT_INDEX: u16 = 1;
    const DATA_INDEX: u16 = 2;
    const BSS_INDEX: u16 = 3;
    const SHSTRTAB_INDEX: u16 = 4;
    const SYMTAB_INDEX: u16 = 5;
    const STRTAB_INDEX: u16 = 6;
    // 当为可执行文件时, base address
    const BASE_ADDRESS: u32 = 0x8048000;

    /// 根据 .text 与 .data 的大小计算 .text, .data, .bss 的虚拟地址, 用于链接使用
    pub fn calc_addresses(text_size: u32, data_size: u32) -> (u32, u32, u32) {
        let header_size = size_of::<Ehdr>() + 3 * size_of::<Phdr>();
        let text_off = align(header_size, 0x1000);
        let text_addr = Self::BASE_ADDRESS + text_off as u32;
        let data_addr = align(text_addr as usize + text_size as usize, 0x1000) as u32;
        let bss_addr = data_addr + data_size;
        (text_addr, data_addr, bss_addr)
    }

    pub fn new() -> Self {
        Self {
            text: Vec::new(),
            data: Vec::new(),
            bss_size: 0,
            symbols: Vec::new(),
            rel_text: Vec::new(),
            rel_data: Vec::new(),
        }
    }

    pub fn text(&mut self, content: Vec<u8>) -> &mut Self {
        self.text = content;
        self
    }

    pub fn data(&mut self, content: Vec<u8>) -> &mut Self {
        self.data = content;
        self
    }

    pub fn bss_size(&mut self, size: u32) -> &mut Self {
        self.bss_size = size;
        self
    }

    pub fn symbols(&mut self, symbols: Vec<Symbol>) -> &mut Self {
        self.symbols = symbols;
        self
    }

    pub fn rel_text(&mut self, rel: Vec<Relocation>) -> &mut Self {
        self.rel_text = rel;
        self
    }

    pub fn rel_data(&mut self, rel: Vec<Relocation>) -> &mut Self {
        self.rel_data = rel;
        self
    }

    pub fn write_obj(&self, out: &mut File) -> Result<(), ElfError> {
        // .shstrtab
        let shstrtab = StringTable::from(&[
            "", ".bss", ".shstrtab", ".symtab", ".strtab",
            ".rel.text", ".rel.data", // .text 与 .data 也包含其中
        ]);
        // .symtab, .strtab, last local + 1
        let (symtab, strtab, symtab_info) = Self::symbol_table(&self.symbols);
        let mut map: HashMap<String, usize> = HashMap::new();
        for index in 0..symtab.len() {
            let str = strtab.get_string(symtab[index].st_name as usize).unwrap(); // 必然成功
            let dup = map.insert(String::from(str), index);
            if dup.is_some() {
                return Err(ElfError::DuplicateSymbol(String::from(str))); // 符号重复
            }
        }
        // .rel.text
        let rel_text = Self::rel_section(&self.rel_text, &map)?;
        // .rel.data
        let rel_data = Self::rel_section(&self.rel_data, &map)?;

        // offsets
        let text_off = size_of::<Ehdr>(); // align = 1
        let data_off = text_off + self.text.len(); // align = 1
        let bss_off = data_off + self.data.len(); // align = 1
        let shstrtab_off = bss_off; // align = 1
        let shstrtab_end = shstrtab_off + shstrtab.size();
        let sh_off = align(shstrtab_end, 4); // align = 4;
        let sh_padding = sh_off - shstrtab_end;
        let symtab_off = sh_off + 9 * size_of::<Shdr>(); // align = 4;
        let strtab_off = symtab_off + symtab.len() * size_of::<Sym>(); // align = 1
        let strtab_end = strtab_off + strtab.size();
        let rel_text_off = align(strtab_end, 4); // align = 4
        let rel_text_padding = rel_text_off - strtab_end;
        let rel_data_off = rel_text_off + rel_text.len() * size_of::<Rel>(); // align = 4

        // section header table
        let section_header_table: [Shdr; 9] = [
            Shdr { // undef
                sh_name: shstrtab.get_index(&String::from("")).unwrap() as Word,
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
                sh_name: shstrtab.get_index(&String::from(".rel.text")).unwrap() as Word + 4,
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
                sh_name: shstrtab.get_index(&String::from(".rel.data")).unwrap() as Word + 4,
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
                sh_name: shstrtab.get_index(&String::from(".bss")).unwrap() as Word,
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
                sh_name: shstrtab.get_index(&String::from(".shstrtab")).unwrap() as Word,
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
                sh_name: shstrtab.get_index(&String::from(".symtab")).unwrap() as Word,
                sh_type: SHT_SYMTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: symtab_off as Off,
                sh_size: (symtab.len() * size_of::<Sym>()) as Word,
                sh_link: Self::STRTAB_INDEX as Word, // string table
                sh_info: symtab_info, // last local + 1
                sh_addralign: 4,
                sh_entsize: size_of::<Sym>() as Word,
            },
            Shdr { // .strtab
                sh_name: shstrtab.get_index(&String::from(".strtab")).unwrap() as Word,
                sh_type: SHT_STRTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: strtab_off as Off,
                sh_size: strtab.size() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .rel.text
                sh_name: shstrtab.get_index(&String::from(".rel.text")).unwrap() as Word,
                sh_type: SHT_REL,
                sh_flags: SHF_INFO_LINK,
                sh_addr: 0,
                sh_offset: rel_text_off as Off,
                sh_size: (rel_text.len() * size_of::<Rel>()) as Word,
                sh_link: Self::SYMTAB_INDEX as Word, // symbol table
                sh_info: Self::TEXT_INDEX as Word, // .text
                sh_addralign: 4,
                sh_entsize: size_of::<Rel>() as Word,
            },
            Shdr { // .rel.data
                sh_name: shstrtab.get_index(&String::from(".rel.data")).unwrap() as Word,
                sh_type: SHT_REL,
                sh_flags: SHF_INFO_LINK,
                sh_addr: 0,
                sh_offset: rel_data_off as Off,
                sh_size: (rel_data.len() * size_of::<Rel>()) as Word,
                sh_link: Self::SYMTAB_INDEX as Word, // symbol table
                sh_info: Self::DATA_INDEX as Word, // .data
                sh_addralign: 4,
                sh_entsize: size_of::<Rel>() as Word,
            },
        ];

        // elf header
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

        // write to file
        // ELF header
        out.write_all(unsafe { serialize(&header) })?;
        // .text section
        out.write_all(&self.text)?;
        // .data section
        out.write_all(&self.data)?;
        // .shstrtab
        out.write_all(shstrtab.content())?;
        for _ in 0..sh_padding {
            out.write_all(&[0])?;
        }
        // section header table
        out.write_all(unsafe { serialize_slice(&section_header_table) })?;
        // .symtab
        out.write_all(unsafe { serialize_slice(&symtab) })?;
        // .strtab
        out.write_all(strtab.content())?;
        for _ in 0..rel_text_padding {
            out.write_all(&[0])?;
        }
        // .rel.text
        out.write_all(unsafe { serialize_slice(&rel_text) })?;
        // .rel.data
        out.write_all(unsafe { serialize_slice(&rel_data) })?;
        Ok(())
    }

    pub fn write_exec(&mut self, out: &mut File) -> Result<(), ElfError> {
        // .shstrtab
        let shstrtab = StringTable::from(&[
            "", ".text", ".data", ".bss",
            ".shstrtab", ".symtab", ".strtab",
        ]);
        // .symtab, .strtab, last local + 1
        let (symtab, strtab, symtab_info) = Self::symbol_table(&self.symbols);
        let mut map: HashMap<String, usize> = HashMap::new();
        for index in 0..symtab.len() {
            let str = strtab.get_string(symtab[index].st_name as usize).unwrap(); // 必然成功
            let dup = map.insert(String::from(str), index);
            if dup.is_some() {
                return Err(ElfError::DuplicateSymbol(String::from(str))); // 符号重复
            }
        }
        // 不支持重定位表
        if self.rel_text.len() != 0 || self.rel_data.len() != 0 {
            return Err(ElfError::ExecutableRelocation);
        }

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
        let strtab_off = symtab_off + symtab.len() * size_of::<Sym>(); // align = 1
        // entry
        let entry = match map.get("_start") {
            Some(index) => *index,
            None => return Err(ElfError::MissingSymbol(String::from("_start"))),
        };
        let entry = symtab[entry];
        if entry.st_shndx != Self::TEXT_INDEX {
            panic!("_start is not in .text");
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
                sh_name: shstrtab.get_index(&String::from("")).unwrap() as Word,
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
                sh_name: shstrtab.get_index(&String::from(".text")).unwrap() as Word,
                sh_type: SHT_PROGBITS,
                sh_flags: SHF_ALLOC | SHF_EXECINSTR,
                sh_addr: text_addr,
                sh_offset: text_off as Off,
                sh_size: self.text.len() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .data
                sh_name: shstrtab.get_index(&String::from(".data")).unwrap() as Word,
                sh_type: SHT_PROGBITS,
                sh_flags: SHF_ALLOC | SHF_WRITE,
                sh_addr: data_addr,
                sh_offset: data_off as Off,
                sh_size: self.data.len() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .bss
                sh_name: shstrtab.get_index(&String::from(".bss")).unwrap() as Word,
                sh_type: SHT_NOBITS,
                sh_flags: SHF_ALLOC | SHF_WRITE,
                sh_addr: bss_addr,
                sh_offset: bss_off as Off,
                sh_size: self.bss_size,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            Shdr { // .shstrtab
                sh_name: shstrtab.get_index(&String::from(".shstrtab")).unwrap() as Word,
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
                sh_name: shstrtab.get_index(&String::from(".symtab")).unwrap() as Word,
                sh_type: SHT_SYMTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: symtab_off as Off,
                sh_size: (symtab.len() * size_of::<Sym>()) as Word,
                sh_link: Self::STRTAB_INDEX as Word, // string table
                sh_info: symtab_info, // last local + 1
                sh_addralign: 4,
                sh_entsize: size_of::<Sym>() as Word,
            },
            Shdr { // .strtab
                sh_name: shstrtab.get_index(&String::from(".strtab")).unwrap() as Word,
                sh_type: SHT_STRTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: strtab_off as Off,
                sh_size: strtab.size() as Word,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
        ];

        // elf header
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

        // write to file
        // ELF header
        out.write_all(unsafe { serialize(&header) })?;
        // program header table
        out.write_all(unsafe { serialize_slice(&program_header_table) })?;
        for _ in 0..text_padding {
            out.write_all(&[0])?;
        }
        // .text
        out.write_all(&self.text)?;
        for _ in 0..data_padding {
            out.write_all(&[0])?;
        }
        // .data
        out.write_all(&self.data)?;
        // .shstrtab
        out.write_all(shstrtab.content())?;
        for _ in 0..sh_padding {
            out.write_all(&[0])?;
        }
        // section header table
        out.write_all(unsafe { serialize_slice(&section_header_table) })?;
        // .symtab
        out.write_all(unsafe { serialize_slice(&symtab) })?;
        // .strtab
        out.write_all(strtab.content())?;
        Ok(())
    }

    // 返回符号表, 字符串表, 以及最后一个 local 的 index + 1
    fn symbol_table(symbols: &Vec<Symbol>) -> (Vec<Sym>, StringTable, u32) {
        let mut string_table = StringTable::new();
        let mut local_symbols: Vec<Sym> = Vec::new();
        let mut global_symbols: Vec<Sym> = Vec::new();
        // 符号表先添加一个无效符号
        let index = string_table.add(String::from(""));
        local_symbols.push(Sym {
            st_name: index as Word,
            st_value: 0,
            st_size: 0,
            st_info: st_info!(STB_LOCAL, STT_NOTYPE),
            st_other: 0,
            st_shndx: 0, // undef
        });

        for symbol in symbols {
            let index = string_table.add(symbol.name.clone());
            let mut sym = Sym {
                st_name: index as Word,
                st_value: symbol.value,
                st_size: 0,
                st_info: 0,
                st_other: 0,
                st_shndx: 0,
            };
            match symbol.section {
                ProgramSection::Undef => {
                    sym.st_info = st_info!(STB_GLOBAL, STT_NOTYPE);
                    sym.st_shndx = 0; // undef
                    global_symbols.push(sym);
                }
                sec @ _ => {
                    sym.st_shndx = match sec {
                        ProgramSection::Text => Self::TEXT_INDEX,
                        ProgramSection::Data => Self::DATA_INDEX,
                        ProgramSection::Bss => Self::BSS_INDEX,
                        _ => 0,
                    };
                    sym.st_value = symbol.value;
                    if symbol.is_global {
                        sym.st_info = st_info!(STB_GLOBAL, STT_NOTYPE);
                        global_symbols.push(sym);
                    } else {
                        sym.st_info = st_info!(STB_LOCAL, STT_NOTYPE);
                        local_symbols.push(sym)
                    }
                }
            }
        }

        let local_len = local_symbols.len();
        local_symbols.append(&mut global_symbols);
        (local_symbols, string_table, local_len as u32)
    }

    fn rel_section(rels: &Vec<Relocation>, map: &HashMap<String, usize>) -> Result<Vec<Rel>, ElfError> {
        let mut ret: Vec<Rel> = Vec::with_capacity(rels.len());
        for rel in rels {
            let symbol = match map.get(&rel.symbol) {
                Some(symbol) => *symbol,
                None => return Err(ElfError::MissingSymbol(rel.symbol.clone())),
            };
            let r_info: Word = if rel.is_relative {
                r_info!(symbol, R_386_PC32)
            } else {
                r_info!(symbol, R_386_32)
            };
            let rel = Rel {
                r_offset: rel.offset,
                r_info,
            };
            ret.push(rel);
        }
        Ok(ret)
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
    fn get_index(&self, str: &String) -> Option<usize> {
        self.index_map.get(str).copied()
    }

    fn get_string(&self, index: usize) -> Option<&str> {
        let mut end = index;
        for ch in &self.content[index..] {
            if *ch == 0 {
                return std::str::from_utf8(&self.content[index..end]).ok();
            }
            end += 1;
        }
        None
    }

    fn content(&self) -> &[u8] {
        &self.content
    }

    fn size(&self) -> usize {
        self.content.len()
    }
}

/// 表示一个符号
#[derive(Clone)]
pub struct Symbol {
    pub name: String,
    pub value: u32,
    pub section: ProgramSection,
    pub is_global: bool,
}

impl Symbol {
    pub fn new(
        name: String,
        value: u32,
        section: ProgramSection,
        is_global: bool
    ) -> Self {
        Self {
            name,
            value,
            section,
            is_global,
        }
    }
}

// 表示一条重定位信息
#[derive(Clone)]
pub struct Relocation {
    pub offset: u32,
    pub symbol: String,
    pub is_relative: bool
}

impl Relocation {
    pub fn new(
        offset: u32,
        symbol: String,
        is_relative: bool
    ) -> Self {
        Self {
            offset,
            symbol,
            is_relative,
        }
    }
}

// 表示 .text, .data 或 .bss
#[derive(Debug)]
#[derive(Clone, Copy)]
#[derive(PartialEq)]
pub enum ProgramSection {
    Undef,
    Text,
    Data,
    Bss
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

        let mut obj = ElfWriter::new();
        obj.text(code)
            .symbols(vec![
                Symbol::new(String::from("_start"),  0, ProgramSection::Text, true)
            ]);

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file).unwrap();
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

        let mut obj = ElfWriter::new();
        obj.text(code)
            .data(data)
            .symbols(vec![
                Symbol::new(String::from("_start"),  0, ProgramSection::Text, true),
                Symbol::new(String::from("num"),  1, ProgramSection::Data, false),
            ])
            .rel_text(vec![
                Relocation::new(7, String::from("num"), false)
            ]);

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file).unwrap();
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

        let mut obj = ElfWriter::new();
        obj.text(code)
            .bss_size(bss_size)
            .symbols(vec![
                Symbol::new(String::from("_start"), 0, ProgramSection::Text, true),
                Symbol::new(String::from("num"), 0, ProgramSection::Bss, false),
            ])
            .rel_text(vec![
                Relocation::new(7, String::from("num"), false)
            ]);

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file).unwrap();
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

        let mut obj = ElfWriter::new();
        obj.text(code)
            .symbols(vec![
                Symbol::new(String::from("_start"), 0, ProgramSection::Text, true),
                Symbol::new(String::from("exit"), code_start.len() as u32, ProgramSection::Text, false),
            ])
            .rel_text(vec![
                Relocation::new(code_start.len() as u32 - 4, String::from("exit"), true)
            ]);

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write_obj(&mut file).unwrap();

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

        let mut exec = ElfWriter::new();
        exec.text(code)
            .symbols(vec![
                Symbol::new(String::from("_start"),  0, ProgramSection::Text, true)
            ]);

        let mut file = File::create("test").expect("fail to create a file");
        let mut perms = file.metadata().unwrap().permissions();
        perms.set_mode(0o755); // rwx r-x r-x
        file.set_permissions(perms).unwrap();
        exec.write_exec(&mut file).unwrap();
        // $ ./test; echo $?
        // 2
    }
}
