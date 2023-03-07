mod common;
mod ast;
mod config;
mod parser;
mod generator;

use std::{fs::File, io::BufReader};

use parser::Parser;
use generator::Generator;

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
    let input = File::open(&input)
        .expect(&format!("can't open file {}", input));
    let mut output = File::create(&output)
        .expect(&format!("can't create file {}", output));

    let mut parser = Parser::new(BufReader::new(input));
    let ast = parser.build_ast().expect("can't build ast");
    let mut generator = Generator::new();
    ast.run_visitor(&mut generator).expect("can't generate infomation");
    generator.write_obj(&mut output).expect("can't write to object file");
}
