mod common;
mod table;
mod data;

use crate::{ast::{Node, Visitor, ProgramNode, ProgramItem, InstructionNode, LabelNode, PseudoSectionNode, PseudoGlobalNode, PseudoEquNode, PseudoFillNode, PseudoIntegerNode, PseudoStringNode, PseudoCommNode, ValueNode, OperandNode, RegisterNode, MemNode}, common::Size};

use self::{table::{SymbolTable, SymbolKind}, data::Data, common::{Section, Value, Statement, RelocationInfo}};

struct Generator {
    data_section: Vec<Data>,
    table: SymbolTable,
    current_section: Option<Section>,
    offset: u32,
    // 属性
    line: usize, // 行号
}

impl Generator {
    fn new() -> Self {
        Self {
            data_section: Vec::new(),
            table: SymbolTable::new(),
            current_section: None,
            offset: 0,
            line: 0,
        }
    }

    fn generate_data_section(&self) -> (Vec<u8>, Vec<RelocationInfo>) {
        let mut section: Vec<u8> = Vec::new();
        let mut rel: Vec<RelocationInfo> = Vec::new();
        for data in &self.data_section {
            let (mut d, mut r) = data.emit();
            let offset = section.len() as u32;
            for info in &mut r {
                info.offset += offset;
            }
            section.append(&mut d);
            rel.append(&mut r);
        }
        (section, rel)
    }

    fn generate_symbol_table(&self) -> (Vec<(String, u32, Section, bool)>, Vec<String>) {
        self.table.labels_and_externals()
    }

    /// 若 name 为 equ, 则返回其值, 否则返回重定位信息, 并尝试在符号表插入一个外部符号
    fn value_of(&mut self, name: &String, addend: u32, is_relative: bool) -> Value {
        match self.table.get_symbol(name) {
            Some(SymbolKind::Equ(value)) => Value::Integer(value),
            _ => {
                self.table.insert_external(name.clone());
                Value::Symbol(name.clone(), addend, is_relative)
            },
        }
    }

    // 将 ValueNode 转为 Value, 只有在为整数或 equ 时才返回值, 否则返回重定位信息, 并尝试在符号表插入一个外部符号
    fn value_of_node(&mut self, node: &ValueNode, addend: u32, is_relative: bool) -> Value {
        match node {
            ValueNode::Integer(value) => Value::Integer(*value),
            ValueNode::Symbol(name) => self.value_of(name, addend, is_relative),
        }
    }
}

/// 访问语法树, 从语法树建立符号表, 生成 data section, bss section 与 text section,
/// 但是标签不替换成值, 仅仅把 equ 替换成值
impl Visitor for Generator {
    type Return = ();

    fn visit_program(&mut self, node: &ProgramNode) -> Self::Return {
        for (item, line) in &node.items {
            self.line = *line;
            match item {
                ProgramItem::PseudoSection(node) => node.accept(self),
                ProgramItem::PseudoGlobal(node) => node.accept(self),
                ProgramItem::PseudoEqu(node) => node.accept(self),
                ProgramItem::PseudoFill(node) => node.accept(self),
                ProgramItem::PseudoInteger(node) => node.accept(self),
                ProgramItem::PseudoString(node) => node.accept(self),
                ProgramItem::PseudoComm(node) => node.accept(self),
                ProgramItem::Instruction(node) => node.accept(self),
                ProgramItem::Label(node) => node.accept(self),
            }
        }
    }

    fn visit_pseudo_section(&mut self, node: &PseudoSectionNode) -> Self::Return {
        self.current_section = Section::from(&node.symbol);
        self.offset = 0;
        if let None = self.current_section {
            panic!("{}: error: .section {}", self.line, node.symbol);
        }
    }

    fn visit_pseudo_global(&mut self, node: &PseudoGlobalNode) -> Self::Return {
        if !self.table.set_global(node.symbol.clone()) {
            panic!("{}: error: .global {}", self.line, node.symbol);
        }
    }

    fn visit_pseudo_equ(&mut self, node: &PseudoEquNode) -> Self::Return {
        if !self.table.insert_equ(node.symbol.clone(), node.value) {
            panic!("{}: error: .equ {}, {}", self.line, node.symbol, node.value);
        }
    }

    fn visit_pseudo_fill(&mut self, node: &PseudoFillNode) -> Self::Return {
        if self.current_section != Some(Section::Data) {
            panic!("{}: error: not in .data section", self.line);
        }
        if node.size > 4 {
            println!("{}: warn: size is {} > 4, use as 4", self.line, node.size);
        }
        let value = match self.value_of_node(&node.value, 0, false) {
            Value::Integer(val) => val,
            Value::Symbol(name, _, _) => panic!("{}: error: {} is not a constant", self.line, name),
        };
        let data = Data::new_fill(node.repeat, node.size, value);
        self.offset += data.length();
        self.data_section.push(data);
    }

    fn visit_value(&mut self, node: &ValueNode) -> Self::Return {
        panic!("shouldn't visit ValueNode")
    }

    fn visit_pseudo_integer(&mut self, node: &PseudoIntegerNode) -> Self::Return {
        if self.current_section != Some(Section::Data) {
            panic!("{}: error: not in .data section", self.line);
        }
        let values: Vec<Value> = node.values
            .iter()
            .map(|node| self.value_of_node(node, 0, false))
            .collect();
        let data = Data::new(node.size, values);
        self.offset += data.length();
        self.data_section.push(data);
    }

    fn visit_pseudo_string(&mut self, node: &PseudoStringNode) -> Self::Return {
        if self.current_section != Some(Section::Data) {
            panic!("{}: error: not in .data section", self.line);
        }
        let mut values: Vec<Value> = node.content
            .bytes()
            .map(|val| Value::Integer(val as u32))
            .collect();
        if node.zero_end {
            values.push(Value::Integer(0));
        }
        let data = Data::new(Size::Byte, values);
        self.offset += data.length();
        self.data_section.push(data);
    }

    fn visit_pseudo_comm(&mut self, node: &PseudoCommNode) -> Self::Return {
        panic!("{}: error: not support .comm/.lcomm", self.line);
    }

    fn visit_instruction(&mut self, node: &InstructionNode) -> Self::Return {
        if self.current_section != Some(Section::Text) {
            panic!("{}: error: not in .text section", self.line);
        }
        println!("{}: warn: ignore instruction", self.line);
    }

    fn visit_operand(&mut self, node: &OperandNode) -> Self::Return {
        todo!()
    }

    fn visit_register(&mut self, node: &RegisterNode) -> Self::Return {
        todo!()
    }

    fn visit_mem(&mut self, node: &MemNode) -> Self::Return {
        todo!()
    }

    fn visit_label(&mut self, node: &LabelNode) -> Self::Return {
        let section = match self.current_section {
            Some(section) => section,
            None => panic!("{}: error: lable defination in unknown section", self.line),
        };
        if !self.table.insert_label(node.label.clone(), self.offset, section) {
            panic!("{}: error: unable to create label \"{}\"", self.line, node.label);
        }
    }

}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_data_section() {
        let code = r#"
        .section .data
        arr: .int 1, 3, 5, 7
        byte_arr: .byte 1, 3, 5, 7
        str_addr: .int str
        str: .string "Hello, world!"
        ones: .fill 3, 3, 1
        ptr: .int extern_addr
        .section .text
        .global main
        main:
        "#.trim();
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        let ast = parser.build_ast().unwrap();

        let mut generator = Generator::new();
        ast.run_visitor(&mut generator);

        let (data_sec, data_rel) = generator.generate_data_section();
        let (labels, externals) = generator.generate_symbol_table();

        println!();
        println!("symbol table is:");
        println!("labels:");
        println!("offset\tsection\tglobal\tname");
        for (name, offset, section, is_global) in labels {
            println!("0x{:x}\t{}\t{}\t{}", offset, section.name(), is_global, name);
        }
        println!("externals:");
        for name in externals {
            println!("{name}");
        }

        println!();
        println!(".data section is:");
        println!("{data_sec:?}");

        println!();
        println!(".data relocation table is:");
        println!("offset\tname\tis_relative");
        for rel in data_rel {
            println!("0x{:x}\t{}\t{}", rel.offset, rel.name, rel.is_relative);
        }
    }
}