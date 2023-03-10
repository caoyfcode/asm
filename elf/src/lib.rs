#[macro_use]
mod elf_h; // 根据 elf.h 编写

/// Object File (Reloacable ELF)
///
/// +----------------------+
/// |      ELF Header      |
/// +----------------------+
/// |        .text         |
/// +----------------------+
/// |        .data         |
/// +----------------------+
/// |        .bss          | 大小为 0
/// +----------------------+
/// |      .shstrtab       |
/// +----------------------+
/// | section header table |
/// +----------------------+
/// |       .symtab        |
/// +----------------------+
/// |       .strtab        |
/// +----------------------+
/// |      .rel.text       |
/// +----------------------+
/// |      .rel.data       |
/// +----------------------+

/// section header table
///
/// +----------------------+
/// |        UNDEF         | 0
/// +----------------------+
/// |        .text         | 1
/// +----------------------+
/// |       .rel.text      | 2
/// +----------------------+
/// |        .data         | 3
/// +----------------------+
/// |       .rel.data      | 4
/// +----------------------+
/// |         .bss         | 5
/// +----------------------+
/// |      .shstrtab       | 6
/// +----------------------+
/// |       .symtab        | 7
/// +----------------------+
/// |       .strtab        | 8
/// +----------------------+

use std::{collections::HashMap, fs::File, mem::size_of, io::Write};

pub use elf_h::*;


/// 表示目标文件
pub struct Obj {
    text: Vec<u8>, // .text
    data: Vec<u8>, // .data
    bss_size: Word, // .bss
    symbol_table: (Vec<Sym>, Vec<Sym>), // .symtab (locals, globals)
    string_table: StringTable, // .strtab
    text_rel: Vec<Rel>, // .rel.text
    data_rel: Vec<Rel>, // .rel.data

    symbol_index: Option<HashMap<usize, usize>>, // name -> index, name 指的是 string_table 的 index, index 指的是 symbol table 的 idnex
}

impl Obj {
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
                ".text" => 1,
                ".data" => 3,
                ".bss" => 5,
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

    /// 在重定位表添加一个表项(调用时必须保证不再调用 add_symbol)
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

    // 构造 symbol_index, 之后符号表就固定了
    // 1. 在调用此之前, 不可添加重定位表
    // 2. 之后, 不可添加符号表
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

    pub fn write(&self, out: &mut File) {
        // .shstrtab
        let shstrtab = StringTable::from(&[
            "", ".bss", ".shstrtab", ".symtab", ".strtab",
            ".rel.text", ".rel.data", // .text 与 .data 也包含其中
        ]);

        // offsets
        let text_off = size_of::<Ehdr>();
        let data_off = text_off + self.text.len(); // align = 1
        let bss_off = data_off + self.data.len(); // align = 1
        let shstrtab_off = bss_off;
        let sh_off = shstrtab_off + shstrtab.size();
        let symtab_off = sh_off + 9 * size_of::<Shdr>();
        let strtab_off = symtab_off +
            (self.symbol_table.0.len() +
            self.symbol_table.1.len()) * size_of::<Sym>();
        let rel_text_off = strtab_off + self.string_table.size();
        let rel_data_off = rel_text_off + self.text_rel.len() * size_of::<Rel>();

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
            Shdr { // .rel.text
                sh_name: shstrtab.get(&String::from(".rel.text")).unwrap() as Word,
                sh_type: SHT_REL,
                sh_flags: SHF_INFO_LINK,
                sh_addr: 0,
                sh_offset: rel_text_off as Off,
                sh_size: (self.text_rel.len() * size_of::<Rel>()) as Word,
                sh_link: 7, // symbol table
                sh_info: 1, // .text
                sh_addralign: 4,
                sh_entsize: size_of::<Rel>() as Word,
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
            Shdr { // .rel.data
                sh_name: shstrtab.get(&String::from(".rel.data")).unwrap() as Word,
                sh_type: SHT_REL,
                sh_flags: SHF_INFO_LINK,
                sh_addr: 0,
                sh_offset: rel_data_off as Off,
                sh_size: (self.data_rel.len() * size_of::<Rel>()) as Word,
                sh_link: 7, // symbol table
                sh_info: 3, // .data
                sh_addralign: 4,
                sh_entsize: size_of::<Rel>() as Word,
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
                sh_link: 8, // string table
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
            e_shstrndx: 6,
        };
        // ELF header
        out.write_all(unsafe { serialize(&header) }).unwrap();
        // .text section
        out.write_all(&self.text).unwrap();
        // .data section
        out.write_all(&self.data).unwrap();
        // .shstrtab
        out.write_all(shstrtab.content()).unwrap();
        // section header table
        out.write_all(unsafe { serialize_slice(&section_header_table) }).unwrap();
        // .symtab
        out.write_all(unsafe { serialize_slice(&self.symbol_table.0) }).unwrap(); // local
        out.write_all(unsafe { serialize_slice(&self.symbol_table.1) }).unwrap(); // global
        // .strtab
        out.write_all(self.string_table.content()).unwrap();
        // .rel.text
        out.write_all(unsafe { serialize_slice(&self.text_rel) }).unwrap();
        // .rel.data
        out.write_all(unsafe { serialize_slice(&self.data_rel) }).unwrap();
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
mod tests {
    use super::*;

    #[test]
    fn test_simple_exit() {
        let code: Vec<u8> = vec![
            0xb8, 0x01, 0x00, 0x00, 0x00, // mov $1, %eax ; 调用号 1: exit
            0xbb, 0x02, 0x00, 0x00, 0x00, // mov $2, %ebx ; 参数 1: 2
            0xcd, 0x80 // int $0x80
        ];
        let mut obj = Obj::new();
        obj.set_section_content(".text", code).unwrap();
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write(&mut file);
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
        let mut obj = Obj::new();
        obj.set_section_content(".text", code).unwrap();
        obj.set_section_content(".data", data).unwrap();
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        obj.add_symbol(String::from("num"), false, 1, ".data", false).unwrap();
        obj.add_relocation(7, ".text", &String::from("num"), false).unwrap();

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write(&mut file);
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
        let mut obj = Obj::new();
        obj.set_section_content(".text", code).unwrap();
        obj.set_bss_size(bss_size);
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        obj.add_symbol(String::from("num"), false, 0, ".bss", false).unwrap();
        obj.add_relocation(7, ".text", &String::from("num"), false).unwrap();

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write(&mut file);
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

        let mut obj = Obj::new();
        obj.set_section_content(".text", code).unwrap();
        obj.add_symbol(String::from("_start"), false, 0, ".text", true).unwrap();
        obj.add_symbol(String::from("exit"), false, code_start.len() as u32, ".text", false).unwrap();
        obj.add_relocation(code_start.len() as u32 - 4, ".text", &String::from("exit"), true).unwrap(); // 添加相对重定位

        let mut file = File::create("test.o").expect("fail to create a file");
        obj.write(&mut file);

        // $ ld -melf_i386 -o test test.o
        // $ ./test
        // $ echo $?
        // 3
    }
}
