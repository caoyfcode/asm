use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::io::{Seek, SeekFrom};
use std::mem::size_of;

use elf::{Symbol, Relocation, ProgramSection, ElfWriter};
use elf::elf_h::*;
use elf::{r_sym, r_type, st_bind, st_type};

// 检查是否相对, 不相等就 return Err(String)
macro_rules! check_eq {
    ($left:expr, $right:expr, $msg:expr) => {
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    return Err(String::from($msg));
                }
            }
        }
    };
}

/// 表示从目标文件中读取的信息
pub struct Obj {
    pub text: Vec<u8>,
    pub data: Vec<u8>,
    pub bss_size: usize,
    pub symbols: Vec<Symbol>,
    rel_text: Vec<Relocation>,
    rel_data: Vec<Relocation>,

    symbol_map: HashMap<String, usize>, // 符号名字到下标的映射
}

impl Obj {
    pub fn parse(input: &mut File) -> Result<Self, String> {
        // header
        let header = read_bytes(input, 0, size_of::<Ehdr>());
        let header: &Ehdr = unsafe { deserialize(&header).unwrap() };
        check_header(header)?;
        // section header table
        let shtab = read_bytes(input, header.e_shoff as u64, size_of::<Shdr>() * header.e_shnum as usize);
        let shtab: &[Shdr] = unsafe { deserialize_slice(&shtab).unwrap() };
        // .shstrtab
        let shstrtab_sh = shtab[header.e_shstrndx as usize];
        check_eq!(shstrtab_sh.sh_type, SHT_STRTAB, "Section Header String Table is not SHT_STRTAB type");
        let shstrtab = read_bytes(input, shstrtab_sh.sh_offset as u64, shstrtab_sh.sh_size as usize);
        // other sections
        let mut sec_indices: HashMap<String, usize> = HashMap::new();
        let mut index: usize = 0;
        for sh in shtab {
            if sh.sh_type == SHT_NULL {
                index += 1;
                continue;
            }
            match str_in_table(&shstrtab, sh.sh_name as usize) {
                Some(name @ (
                    ".text" | ".data" | ".bss"
                    | ".symtab" | ".strtab" | ".rel.text" | ".rel.data"
                )) => {
                    if sec_indices.insert(String::from(name), index).is_some() {
                        return Err(format!("duplicate section {}", name));
                    }
                }
                _ => (),
            }
            index += 1;
        }

        let text: Vec<u8> = if let Some(index) = sec_indices.get(".text") {
            let sh = shtab[*index];
            check_eq!(sh.sh_type, SHT_PROGBITS, ".text is not SHT_PROGBITS type");
            check_eq!(sh.sh_flags, SHF_ALLOC | SHF_EXECINSTR, ".text flag is not SHF_ALLOC | SHF_EXECINSTR");
            read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize)
        } else {
            Vec::new()
        };
        let data: Vec<u8> = if let Some(index) = sec_indices.get(".data") {
            let sh = shtab[*index];
            check_eq!(sh.sh_type, SHT_PROGBITS, ".data is not SHT_PROGBITS type");
            check_eq!(sh.sh_flags, SHF_ALLOC | SHF_WRITE, ".data flag is not SHF_ALLOC | SHF_WRITE");
            read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize)
        } else {
            Vec::new()
        };
        let bss_size: usize = if let Some(index) = sec_indices.get(".bss") {
            let sh = shtab[*index];
            check_eq!(sh.sh_type, SHT_NOBITS, ".data is not SHT_PROGBITS type");
            check_eq!(sh.sh_flags, SHF_ALLOC | SHF_WRITE, ".bss flag is not SHF_ALLOC | SHF_WRITE");
            sh.sh_size as usize
        } else {
            0
        };
        let mut symbols: Vec<Symbol> = Vec::new();
        let mut symbol_map: HashMap<String, usize> = HashMap::new();
        let mut rel_text: Vec<Relocation> = Vec::new();
        let mut rel_data: Vec<Relocation> = Vec::new();
        if let Some(symtab_index) = sec_indices.get(".symtab") {
            let symtab_sh = shtab[*symtab_index];
            check_eq!(symtab_sh.sh_type, SHT_SYMTAB, ".symtab is not SHT_SYMTAB type");
            let strtab_index = match sec_indices.get(".strtab") {
                Some(index) => index,
                None => return Err("no .strtab section".to_string()),
            };
            check_eq!(symtab_sh.sh_link, *strtab_index as Word, ".symtab does not link .strtab");
            check_eq!(symtab_sh.sh_entsize, size_of::<Sym>() as Word, ".symtab entry size error");
            if symtab_sh.sh_size as usize % size_of::<Sym>() != 0 {
                return Err(".symtab size error".to_string());
            }
            let symtab_raw: Vec<u8> = read_bytes(input, symtab_sh.sh_offset as u64, symtab_sh.sh_size as usize);
            let symtab_raw: &[Sym] = unsafe { deserialize_slice(&symtab_raw).unwrap() };
            // .strtab
            let strtab_sh = shtab[*strtab_index];
            check_eq!(strtab_sh.sh_type, SHT_STRTAB, ".strtab is not SHT_STRTAB type");
            let strtab: Vec<u8> = read_bytes(input, strtab_sh.sh_offset as u64, strtab_sh.sh_size as usize);
            // .symtab
            let text_index = sec_indices.get(".text").copied();
            let data_index = sec_indices.get(".data").copied();
            let bss_index = sec_indices.get(".bss").copied();
            for sym in &symtab_raw[1..] { // 0 为 undef, 忽略
                let name = match str_in_table(&strtab, sym.st_name as usize) {
                    Some(str) => str,
                    None => return Err("symbol in .symtab can't find name in .strtab".to_string()),
                };

                let bind = st_bind!(sym.st_info);
                let st_type = st_type!(sym.st_info);
                let is_global = match bind {
                    STB_LOCAL => false,
                    STB_GLOBAL => true,
                    _ => return Err(format!("unsupported st_bind: {}", bind)),
                };
                check_eq!(st_type, STT_NOTYPE, "symbol is not STT_NOTYPE type");
                check_eq!(sym.st_size, 0, "symbol size is not 0");
                let is_undef = sym.st_shndx == 0;
                let section = if !is_undef {
                    if text_index == Some(sym.st_shndx as usize) {
                        ProgramSection::Text
                    } else if data_index == Some(sym.st_shndx as usize) {
                        ProgramSection::Data
                    } else if bss_index == Some(sym.st_shndx as usize) {
                        ProgramSection::Bss
                    } else {
                        return Err(format!("symbol {} in unsupported section", name));
                    }
                } else {
                    ProgramSection::Undef
                };
                if symbol_map.insert(String::from(name), symbols.len()).is_some() {
                    return Err(format!("duplicate symbol {}", name));
                }
                symbols.push(Symbol::new(String::from(name), sym.st_value, section, is_global));
            }
            // .rel.text
            if let Some(rel_index) = sec_indices.get(".rel.text") {
                let sh = shtab[*rel_index];
                check_eq!(sh.sh_type, SHT_REL, ".rel.text is not SHT_REL type");
                check_eq!(sh.sh_link as usize, *symtab_index, ".rel.text does not link .symtab");
                check_eq!(Some(sh.sh_info as usize), text_index, ".rel.text sh_info is not .text");
                check_eq!(sh.sh_entsize, size_of::<Rel>() as Word, ".rel.text entry size error");
                if sh.sh_size as usize % size_of::<Rel>() != 0 {
                    return Err(".rel.text size error".to_string());
                }
                let raw = read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize);
                let raw: &[Rel] = unsafe { deserialize_slice(&raw).unwrap() };
                for rel in raw {
                    let symbol = r_sym!(rel.r_info);
                    let r_type = r_type!(rel.r_info);
                    if symbol == 0 {
                        return Err("reference to symbol 0 in .rel.text".to_string());
                    }
                    let symbol = symbols[symbol as usize - 1].name.clone(); // 减 1 是因为没有放 0
                    let is_relative = match r_type as u8 {
                        R_386_32 => false,
                        R_386_PC32 => true,
                        _ => return Err("unsupported reloaction type".to_string()),
                    };
                    rel_text.push(Relocation::new(rel.r_offset, symbol, is_relative));
                }
            }
            // .rel.data
            if let Some(rel_index) = sec_indices.get(".rel.data") {
                let sh = shtab[*rel_index];
                check_eq!(sh.sh_type, SHT_REL, ".rel.data is not SHT_REL type");
                check_eq!(sh.sh_link as usize, *symtab_index, ".rel.data does not link .symtab");
                check_eq!(Some(sh.sh_info as usize), data_index, ".rel.data sh_info is not .data");
                check_eq!(sh.sh_entsize, size_of::<Rel>() as Word, ".rel.data entry size error");
                if sh.sh_size as usize % size_of::<Rel>() != 0 {
                    return Err(".rel.data size error".to_string());
                }
                let raw = read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize);
                let raw: &[Rel] = unsafe { deserialize_slice(&raw).unwrap() };
                for rel in raw {
                    let symbol = r_sym!(rel.r_info);
                    let r_type = r_type!(rel.r_info);
                    if symbol == 0 {
                        return Err("reference to symbol 0 in .rel.data".to_string());
                    }
                    let symbol = symbols[symbol as usize - 1].name.clone(); // 减 1 是因为没有放 0
                    let is_relative = match r_type as u8 {
                        R_386_32 => false,
                        R_386_PC32 => true,
                        _ => return Err("unsupported reloaction type".to_string()),
                    };
                    rel_data.push(Relocation::new(rel.r_offset, symbol, is_relative));
                }
            }
        }
        Ok(Self {
            text,
            data,
            bss_size,
            symbols,
            rel_text,
            rel_data,
            symbol_map
        })
    }

    /// 链接多个目标文件
    pub fn link(mut objs: Vec<Self>) -> Result<Self, String> {
        // 每个 obj 的 .text, .data, .bss 起始地址在最终 obj 中对应 section 的偏移
        let mut offsets: Vec<(u32, u32, u32)> = Vec::with_capacity(objs.len());
        let mut size = (0u32, 0u32, 0u32); // .text, .data, .bss 大小
        for obj in &objs {
            offsets.push(size);
            size.0 += obj.text.len() as u32;
            size.1 += obj.data.len() as u32;
            size.2 += obj.bss_size as u32;
        }
        let mut ret = Self {
            text: Vec::with_capacity(size.0 as usize),
            data: Vec::with_capacity(size.1 as usize),
            bss_size: size.2 as usize,
            symbols: Vec::new(),
            rel_text: Vec::new(),
            rel_data: Vec::new(),
            symbol_map: HashMap::new(),
        };
        let (text_base, data_base, bss_base) = ElfWriter::calc_addresses(size.0, size.1);
        for index in 0..objs.len() {
            let (text_off, data_off, bss_off) = offsets[index];
            let (text_addr, data_addr, bss_addr) = (text_base + text_off, data_base + data_off, bss_base + bss_off);
            objs[index].handle_relocations(".text", text_addr, data_addr, bss_addr)?;
            objs[index].handle_relocations(".data", text_addr, data_addr, bss_addr)?;
            ret.text.extend(objs[index].text.iter());
            ret.data.extend(objs[index].data.iter());
            for symbol in &objs[index].symbols {
                if symbol.is_global && symbol.section != ProgramSection::Undef {
                    let mut symbol = symbol.clone();
                    symbol.value += match symbol.section {
                        ProgramSection::Text => text_off,
                        ProgramSection::Data => data_off,
                        ProgramSection::Bss => bss_off,
                        ProgramSection::Undef => 0, // can't be here
                    };
                    if ret.symbol_map.insert(symbol.name.clone(), ret.symbols.len()).is_some() { // 存在重复符号
                        return Err(format!("duplicate symbol: {}", symbol.name));
                    }
                    ret.symbols.push(symbol);
                }
            }
            for rel in &objs[index].rel_text {
                let mut rel = rel.clone();
                rel.offset += text_off;
                ret.rel_text.push(rel);
            }
            for rel in &objs[index].rel_data {
                let mut rel = rel.clone();
                rel.offset += data_off;
                ret.rel_data.push(rel);
            }
        }
        ret.handle_relocations(".text", text_base, data_base, bss_base)?;
        ret.handle_relocations(".data", text_base, data_base, bss_base)?;
        Ok(ret)
    }

    /// 处理所有非外部符号的重定位, 并在重定位表项中删除对应项
    fn handle_relocations(&mut self, section: &str, text_addr: u32, data_addr: u32, bss_addr: u32) -> Result<(), String>  {
        let (sec, sec_rels, sec_addr) = match section {
            ".text" => (&mut self.text, &mut self.rel_text, text_addr),
            ".data" => (&mut self.data, &mut self.rel_data, data_addr),
            _ => return Err("unsupported relocation section".to_string()),
        };
        let mut extern_rels: Vec<Relocation> = Vec::new();
        for rel in sec_rels.iter() {
            // 符号的信息
            let index = match self.symbol_map.get(&rel.symbol) {
                Some(index) => index,
                None => return Err(format!("unknown symbol: {}", rel.symbol)),
            };
            let symbol = &self.symbols[*index];
            let symbol_addr = match symbol.section {
                ProgramSection::Text => text_addr + symbol.value,
                ProgramSection::Data => data_addr + symbol.value,
                ProgramSection::Bss => bss_addr + symbol.value,
                ProgramSection::Undef => { // 外部符号
                    extern_rels.push(rel.clone());
                    continue;
                },
            };

            // 重定位的位置
            let rel_addr = sec_addr + rel.offset;
            let index = rel.offset as usize;
            if index >= sec.len() || index + 3 >= sec.len() {
                return Err("relocation is out of section".to_string());
            }

            // 计算重定位的值
            let value: u32 = if rel.is_relative {
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
        *sec_rels = extern_rels;
        Ok(())
    }
}

fn str_in_table(table: &[u8], index: usize) -> Option<&str> {
    let mut end = index;
    for ch in &table[index..] {
        if *ch == 0 {
            return std::str::from_utf8(&table[index..end]).ok();
        }
        end += 1;
    }
    None
}

fn read_bytes(file: &mut File, offset: u64, len: usize) -> Vec<u8> {
    let mut content: Vec<u8> = Vec::with_capacity(len);
    content.resize(len, 0);
    file.seek(SeekFrom::Start(offset)).unwrap();
    let read_len = file.read(&mut content[..]).unwrap();
    if read_len != len {
        panic!("read {} bytes from file offset {} fail", len, offset);
    }
    content
}

fn check_header(header: &Ehdr) -> Result<(), String> {
    check_eq!(
        header.e_ident,
        [
            ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
            ELFCLASS32, ELFDATA2LSB, EV_CURRENT as u8,
            ELFOSABI_NONE, 0,
            0, 0, 0, 0, 0, 0, 0
        ],
        "not a valid ELF file"
    );
    check_eq!(header.e_type, ET_REL, "not a relocation file");
    check_eq!(header.e_machine, EM_386, "not x86/i386 arch");
    check_eq!(header.e_version, EV_CURRENT, "unsupported ELF version");
    check_eq!(header.e_ehsize, size_of::<Ehdr>() as Half, "ELF Header size error");
    check_eq!(header.e_shentsize, size_of::<Shdr>() as Half, "Section Header size error");
    check_eq!(header.e_phnum, 0, "Program Header number is not 0");
    Ok(())
}

unsafe fn deserialize<T: Sized>(src: &[u8]) -> Option<&T> {
    if src.len() != size_of::<T>() {
        return None;
    }
    let p = src.as_ptr() as *const T;
    Some(&*p)
}

unsafe fn deserialize_slice<T: Sized>(src: &[u8]) -> Option<&[T]> {
    let size = size_of::<T>();
    let len = src.len();
    if len % size != 0 {
        return None;
    }
    let num = len / size;
    let p = src.as_ptr() as *const T;
    Some(std::slice::from_raw_parts(p, num))
}
