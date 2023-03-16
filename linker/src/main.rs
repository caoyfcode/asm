use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::io::{Seek, SeekFrom};
use std::mem::size_of;
use std::os::unix::prelude::PermissionsExt;

use elf::*;

fn main() {
    let mut input: Option<String> = None;
    let mut output: Option<String> = None;
    let mut out_flag = false;
    let mut args = std::env::args();
    args.next();
    for arg in args {
        if out_flag {
            if output.is_some() {
                panic!("duplicate output file: {}", arg);
            }
            output = Some(arg);
            out_flag = false;
        } else if &arg == "-o" {
            out_flag = true;
        } else {
            if input.is_some() {
                panic!("duplicate input file: {}", arg);
            }
            input = Some(arg);
        }
    }
    if input.is_none() || output.is_none() {
        panic!("no input file or output file");
    }
    let (input, output) = (input.unwrap(), output.unwrap());
    let mut input = File::open(&input)
        .expect(&format!("can't open file {}", input));
    let mut output = File::create(&output)
        .expect(&format!("can't create file {}", output));
    let mut perms = output.metadata().unwrap().permissions();
    perms.set_mode(0o755); // rwx r-x r-x
    output.set_permissions(perms).unwrap();

    let mut elf = parse_obj(&mut input);
    elf.write_exec(&mut output);
}

fn parse_obj(input: &mut File) -> Elf {
    // header
    let header = read_bytes(input, 0, size_of::<Ehdr>());
    let header: &Ehdr = unsafe { deserialize(&header).unwrap() };
    assert_header(header);
    // section header table
    let shtab = read_bytes(input, header.e_shoff as u64, size_of::<Shdr>() * header.e_shnum as usize);
    let shtab: &[Shdr] = unsafe { deserialize_slice(&shtab).unwrap() };
    // .shstrtab
    let shstrtab_sh = shtab[header.e_shstrndx as usize];
    assert_eq!(shstrtab_sh.sh_type, SHT_STRTAB);
    let shstrtab = read_bytes(input, shstrtab_sh.sh_offset as u64, shstrtab_sh.sh_size as usize);
    // other sections
    let mut sec_indices: HashMap<String, usize> = HashMap::new();
    let mut index: usize = 0;
    for sh in shtab {
        if sh.sh_type == SHT_NULL {
            index += 1;
            continue;
        }
        match str_in_table(&shstrtab, sh.sh_name as usize).expect("can't convert bytes to utf-8 str") {
            name @ (
                ".text" | ".data" | ".bss"
                | ".symtab" | ".strtab" | ".rel.text" | ".rel.data"
            ) => {
                if sec_indices.get(name).is_some() {
                    panic!("duplicate {} section", name);
                }
                sec_indices.insert(String::from(name), index);
            }
            _ => (),
        }
        index += 1;
    }

    let text: Vec<u8> = if let Some(index) = sec_indices.get(".text") {
        let sh = shtab[*index];
        assert_eq!(sh.sh_type, SHT_PROGBITS);
        assert_eq!(sh.sh_flags, SHF_ALLOC | SHF_EXECINSTR);
        read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize)
    } else {
        Vec::new()
    };
    let data: Vec<u8> = if let Some(index) = sec_indices.get(".data") {
        let sh = shtab[*index];
        assert_eq!(sh.sh_type, SHT_PROGBITS);
        assert_eq!(sh.sh_flags, SHF_ALLOC | SHF_WRITE);
        read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize)
    } else {
        Vec::new()
    };
    let bss_size: usize = if let Some(index) = sec_indices.get(".bss") {
        let sh = shtab[*index];
        assert_eq!(sh.sh_type, SHT_NOBITS);
        assert_eq!(sh.sh_flags, SHF_ALLOC | SHF_WRITE);
        sh.sh_size as usize
    } else {
        0
    };
    let mut symbols: Vec<Symbol> = Vec::new();
    let mut rel_text: Vec<Relocation> = Vec::new();
    let mut rel_data: Vec<Relocation> = Vec::new();
    if let Some(symtab_index) = sec_indices.get(".symtab") {
        let symtab_sh = shtab[*symtab_index];
        assert_eq!(symtab_sh.sh_type, SHT_SYMTAB);
        let strtab_index = sec_indices.get(".strtab").expect("no .strtab section");
        assert_eq!(symtab_sh.sh_link, *strtab_index as Word);
        assert_eq!(symtab_sh.sh_entsize, size_of::<Sym>() as Word);
        let symtab_raw: Vec<u8> = read_bytes(input, symtab_sh.sh_offset as u64, symtab_sh.sh_size as usize);
        let symtab_raw: &[Sym] = unsafe { deserialize_slice(&symtab_raw).unwrap() };
        // .strtab
        let strtab_sh = shtab[*strtab_index];
        assert_eq!(strtab_sh.sh_type, SHT_STRTAB);
        let strtab: Vec<u8> = read_bytes(input, strtab_sh.sh_offset as u64, strtab_sh.sh_size as usize);
        // .symtab
        let text_index = sec_indices.get(".text").copied();
        let data_index = sec_indices.get(".data").copied();
        let bss_index = sec_indices.get(".bss").copied();
        for sym in symtab_raw {
            let name = str_in_table(&strtab, sym.st_name as usize).expect("can't find symbol name");
            let bind = st_bind!(sym.st_info);
            let st_type = st_type!(sym.st_info);
            let is_global = match bind {
                STB_LOCAL => false,
                STB_GLOBAL => true,
                _ => panic!("unsupported st_bind"),
            };
            assert_eq!(st_type, STT_NOTYPE);
            assert_eq!(sym.st_size, 0);
            let is_undef = sym.st_shndx == 0;
            let mut sec_name = String::new();
            if !is_undef {
                if text_index == Some(sym.st_shndx as usize) {
                    sec_name = String::from(".text");
                } else if data_index == Some(sym.st_shndx as usize) {
                    sec_name = String::from(".data");
                } else if bss_index == Some(sym.st_shndx as usize) {
                    sec_name = String::from(".bss");
                }else {
                    panic!("symbol {} in unsupported section", name);
                }
            }
            symbols.push(Symbol::new(String::from(name), is_undef, sym.st_value, sec_name, is_global));
        }
        // .rel.text
        if let Some(rel_index) = sec_indices.get(".rel.text") {
            let sh = shtab[*rel_index];
            assert_eq!(sh.sh_type, SHT_REL);
            assert_eq!(sh.sh_link as usize, *symtab_index);
            assert_eq!(Some(sh.sh_info as usize), text_index);
            assert_eq!(sh.sh_entsize, size_of::<Rel>() as Word);
            let raw = read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize);
            let raw: &[Rel] = unsafe { deserialize_slice(&raw).unwrap() };
            for rel in raw {
                let symbol = r_sym!(rel.r_info);
                let r_type = r_type!(rel.r_info);
                let symbol = symbols[symbol as usize].name.clone();
                let is_relative = match r_type as u8 {
                    R_386_32 => false,
                    R_386_PC32 => true,
                    _ => panic!("unsupported reloaction type"),
                };
                rel_text.push(Relocation::new(rel.r_offset, String::from(".text"), symbol, is_relative));
            }
        }
        // .rel.data
        if let Some(rel_index) = sec_indices.get(".rel.data") {
            let sh = shtab[*rel_index];
            assert_eq!(sh.sh_type, SHT_REL);
            assert_eq!(sh.sh_link as usize, *symtab_index);
            assert_eq!(Some(sh.sh_info as usize), data_index);
            assert_eq!(sh.sh_entsize, size_of::<Rel>() as Word);
            let raw = read_bytes(input, sh.sh_offset as u64, sh.sh_size as usize);
            let raw: &[Rel] = unsafe { deserialize_slice(&raw).unwrap() };
            for rel in raw {
                let symbol = r_sym!(rel.r_info);
                let r_type = r_type!(rel.r_info);
                let symbol = symbols[symbol as usize].name.clone();
                let is_relative = match r_type as u8 {
                    R_386_32 => false,
                    R_386_PC32 => true,
                    _ => panic!("unsupported reloaction type"),
                };
                rel_data.push(Relocation::new(rel.r_offset, String::from(".data"), symbol, is_relative));
            }
        }
    }

    let mut elf = Elf::new();
    elf.set_section_content(".text", text);
    elf.set_section_content(".data", data);
    elf.set_bss_size(bss_size as u32);
    for Symbol { name, is_undef, value, sec_name, is_global } in symbols {
        elf.add_symbol(name, is_undef, value, &sec_name, is_global);
    }
    for Relocation { offset, section, symbol, is_relative } in rel_text {
        elf.add_relocation(offset, &section, &symbol, is_relative);
    }
    for Relocation { offset, section, symbol, is_relative } in rel_data {
        elf.add_relocation(offset, &section, &symbol, is_relative);
    }
    elf
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
    assert_eq!(read_len, len);
    content
}

fn assert_header(header: &Ehdr) {
    assert_eq!(
        header.e_ident,
        [
            ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
            ELFCLASS32, ELFDATA2LSB, EV_CURRENT as u8,
            ELFOSABI_NONE, 0,
            0, 0, 0, 0, 0, 0, 0
        ],
    );
    assert_eq!(header.e_type, ET_REL);
    assert_eq!(header.e_machine, EM_386);
    assert_eq!(header.e_version, EV_CURRENT);
    assert_eq!(header.e_ehsize, size_of::<Ehdr>() as Half);
    assert_eq!(header.e_shentsize, size_of::<Shdr>() as Half);
    assert_eq!(header.e_phnum, 0);
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
