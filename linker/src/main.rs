mod obj;

use std::fs::File;
use std::os::unix::prelude::PermissionsExt;

use elf::ElfWriter;

use obj::Obj;

fn main() {
    // parse args
    let mut input: Vec<String> = Vec::new();
    let mut output: Option<String> = None;
    let mut out_flag = false;
    let mut args = std::env::args();
    args.next();
    for arg in args {
        if out_flag {
            out_flag = false;
            output = Some(arg);
        } else if &arg == "-o" {
            out_flag = true;
        } else {
            input.push(arg);
        }
    }
    if input.len() == 0 || output.is_none() {
        println!("error: no input file or output file");
        std::process::exit(-1);
    }
    // open input files and parse into object
    let mut objs: Vec<Obj> = Vec::with_capacity(input.len());
    for name in input {
        let mut file = File::open(&name)
            .unwrap_or_else(|_| {
                println!("error: can't open file {}", name);
                std::process::exit(-1);
            });
        let obj = Obj::parse(&mut file)
            .unwrap_or_else(|e| {
                println!("error: can't parse {} as object file: {}", name, e);
                std::process::exit(-1);
            });
        objs.push(obj);
    }
    let obj = Obj::link(objs)
        .unwrap_or_else(|e| {
            println!("error: link objects fail: {}", e);
            std::process::exit(-1);
        });
    // create output file
    let output = output.unwrap();
    let mut output = File::create(&output)
        .unwrap_or_else(|_| {
            println!("errror: can't create file {}", output);
            std::process::exit(-1);
        });
    let mut perms = output.metadata().unwrap().permissions();
    perms.set_mode(0o755); // rwx r-x r-x
    output.set_permissions(perms).unwrap();
    // write to executable file
    ElfWriter::new()
        .text(obj.text)
        .data(obj.data)
        .bss_size(obj.bss_size as u32)
        .symbols((obj.symbols).to_vec())
        .write_exec(&mut output)
        .unwrap_or_else(|e| {
            println!("error: can't write to executable file: {}", e);
            std::process::exit(-1);
        });
}
