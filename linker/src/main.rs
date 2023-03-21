mod obj;

use std::fs::File;
use std::os::unix::prelude::PermissionsExt;

use elf::ElfWriter;

use obj::Obj;

fn main() {
    let mut input: Vec<String> = Vec::new();
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
            input.push(arg);
        }
    }
    if input.len() == 0 || output.is_none() {
        panic!("no input file or output file");
    }

    let mut objs: Vec<Obj> = Vec::with_capacity(input.len());
    for name in input {
        let mut file = File::open(&name)
            .expect(&format!("can't open file {}", name));
        let obj = Obj::parse(&mut file)
            .expect(&format!("parse {} into object file fail", name));
        objs.push(obj);
    }
    let obj = Obj::link(objs).expect("link object files fails"); // 链接之后的 object

    let output = output.unwrap();
    let mut output = File::create(&output)
        .expect(&format!("can't create file {}", output));
    let mut perms = output.metadata().unwrap().permissions();
    perms.set_mode(0o755); // rwx r-x r-x
    output.set_permissions(perms).unwrap();

    ElfWriter::new()
        .text(obj.text)
        .data(obj.data)
        .bss_size(obj.bss_size as u32)
        .symbols((obj.symbols).to_vec())
        .write_exec(&mut output)
        .expect("generate executable file fail");
}
