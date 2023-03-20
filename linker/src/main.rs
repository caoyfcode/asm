mod obj;

use std::fs::File;
use std::os::unix::prelude::PermissionsExt;

use elf::ElfWriter;

use obj::Obj;

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

    let mut obj = Obj::parse(&mut input).expect("parse object file fail");
    obj.link_self().expect("link self fail");
    ElfWriter::new()
        .text(obj.text)
        .data(obj.data)
        .bss_size(obj.bss_size as u32)
        .symbols((&obj.symbols[1..]).to_vec())
        .write_exec(&mut output)
        .expect("generate executable file fail");
}
