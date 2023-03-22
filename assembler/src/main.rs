mod common;
mod ast;
mod config;
mod parser;
mod generator;

use std::{fs::File, io::BufReader};

use elf::ElfWriter;
use parser::Parser;
use generator::Generator;

fn main() {
    // parse args
    let mut input: Option<String> = None;
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
            if input.is_some() {
                println!("error: too many input files");
                std::process::exit(-1);
            }
            input = Some(arg);
        }
    }
    if input.is_none() || output.is_none() {
        println!("error: no input file or output file");
        std::process::exit(-1);
    }
    // open files
    let (input, output) = (input.unwrap(), output.unwrap());
    let input = File::open(&input)
        .unwrap_or_else(|_| {
            println!("error: can't open file {}", input);
            std::process::exit(-1);
        });
    let mut output = File::create(&output)
        .unwrap_or_else(|_| {
            println!("errror: can't create file {}", output);
            std::process::exit(-1);
        });
    // parse file
    let mut parser = Parser::new(BufReader::new(input));
    let ast = parser.build_ast()
        .unwrap_or_else(|e| {
            println!("{}", e);
            std::process::exit(-1);
        });
    let mut generator = Generator::new();
    ast.run_visitor(&mut generator)
        .unwrap_or_else(|e| {
            println!("{}", e);
            std::process::exit(-1);
        });
    // write to object file
    let (data_sec, data_rel) = generator.generate_data_section();
    let (text_sec, text_rel) = generator.generate_text_section();
    let bss_size = generator.generate_bss_section_size();
    let symbols = generator.generate_symbol_table();
    ElfWriter::new()
        .text(text_sec)
        .data(data_sec)
        .bss_size(bss_size)
        .symbols(symbols)
        .rel_text(text_rel)
        .rel_data(data_rel)
        .write_obj(&mut output)
        .unwrap_or_else(|e| {
            println!("error: can't write to object file: {}", e);
            std::process::exit(-1);
        });
}
