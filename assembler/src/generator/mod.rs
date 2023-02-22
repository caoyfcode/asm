mod table;
mod data;

use crate::ast::{Node, Visitor, ProgramNode, ProgramItem, InstructionNode, LabelNode, PseudoSectionNode, PseudoGlobalNode, PseudoEquNode, PseudoFillNode, PseudoIntegerNode, PseudoStringNode, PseudoCommNode, ValueNode, OperandNode, RegisterNode, MemNode};
use crate::common::Size;
use crate::config::{self, OperandEncoding, RegisterKind, RegisterInfo};

use table::{SymbolTable, SymbolKind};
use data::Data;

/// .section 声明的节
#[derive(PartialEq, Clone, Copy)]
enum Section {
    Text,
    Data,
    Bss,
}

impl Section {
    fn from(name: &str) -> Option<Self> {
        match name {
            ".text" => Some(Self::Text),
            ".data" => Some(Self::Data),
            ".bss" => Some(Self::Bss),
            _ => None,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            Section::Text => ".text",
            Section::Data => ".data",
            Section::Bss => ".bss",
        }
    }
}

/// 表示一条指令或数据定义
trait Statement {
    fn length(&self) -> u32;
    fn emit(&self) -> (Vec<u8>, Vec<RelocationInfo>);
}

/// 用于在 Statement 中表示数值或标签或外部符号
enum Value {
    Integer(u32), // 可以确定的数值
    Symbol(String, u32, bool), // 标签或外部符号, (name, addend, is_relative)
}

struct RelocationInfo {
    offset: u32, // 重定位地址
    name: String, // 重定位符号名
    is_relative: bool, // 是则 R_386_PC32, 否则 R_386_32
}

/// 用于访问语法树生成目标文件
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

    fn visit_value(&mut self, _node: &ValueNode) -> Self::Return {
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

    fn visit_pseudo_comm(&mut self, _node: &PseudoCommNode) -> Self::Return {
        panic!("{}: error: not support .comm/.lcomm", self.line);
    }

    fn visit_instruction(&mut self, node: &InstructionNode) -> Self::Return {
        if self.current_section != Some(Section::Text) {
            panic!("{}: error: not in .text section", self.line);
        }
        let infos = match config::infos_of_instruction(&node.mnemonic) {
            Some(infos) => infos,
            None => panic!("{}: error: no such instruction \"{}\"", self.line, node.mnemonic)
        };
        for info in infos {
            if info.operand_encoding.matching(&node.operands) {
                println!("{}: operand encoding is {:?}", self.line, info.operand_encoding);
                return;
            }
        }
        panic!("{}: error: unknown operands of \"{}\"", self.line, node.mnemonic)
    }

    fn visit_operand(&mut self, _node: &OperandNode) -> Self::Return {
        panic!("shouldn't visit OperandNode")
    }

    fn visit_register(&mut self, _node: &RegisterNode) -> Self::Return {
        panic!("shouldn't visit RegisterNode")
    }

    fn visit_mem(&mut self, _node: &MemNode) -> Self::Return {
        panic!("shouldn't visit MemNode")
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

impl OperandEncoding {
    fn matching(&self, operands: &Vec<OperandNode>) -> bool {
        match self {
            OperandEncoding::Zero => operands.len() == 0,
            OperandEncoding::Opcode => operands.len() == 1 && operands[0].is_reg(),
            OperandEncoding::ImpliedSreg(name) => operands.len() == 1 && operands[0].is_register_of_name(name),
            OperandEncoding::Rm => operands.len() == 1 && operands[0].is_rm(),
            OperandEncoding::Reg => operands.len() == 1 && operands[0].is_reg(),
            OperandEncoding::Imm => operands.len() == 1 && operands[0].is_imm(),
            OperandEncoding::Rel => operands.len() == 1 && operands[0].is_imm(),
            OperandEncoding::RegRm => operands.len() == 2 && operands[0].is_reg() && operands[1].is_rm(),
            OperandEncoding::RmReg => operands.len() == 2 && operands[0].is_rm() && operands[1].is_reg(),
            OperandEncoding::MoffsA => operands.len() == 2 && operands[0].is_imm() && operands[1].is_register_a(),
            OperandEncoding::AMoffs => operands.len() == 2 && operands[0].is_register_a() && operands[1].is_imm(),
            OperandEncoding::ImmOpcode => operands.len() == 2 && operands[0].is_imm() && operands[1].is_reg(),
            OperandEncoding::ImmRm => operands.len() == 2 && operands[0].is_imm() && operands[1].is_rm(),
            OperandEncoding::MemReg => operands.len() == 2 && operands[0].is_mem() && operands[1].is_reg(),
            OperandEncoding::ImmA => operands.len() == 2 && operands[0].is_imm() && operands[1].is_register_a(),
            OperandEncoding::SregRm => operands.len() == 2 && operands[0].is_sreg() && operands[1].is_rm(),
            OperandEncoding::RmSreg => operands.len() == 2 && operands[0].is_rm() && operands[1].is_sreg(),
        }
    }

}

impl OperandNode {
    fn get_reg(&self) -> Option<&'static RegisterInfo> {
        if let OperandNode::Register(reg) = self {
            config::info_of_register(&reg.name)
        } else {
            None
        }
    }

    /// 是否是通用寄存器
    fn is_reg(&self) -> bool {
        if let OperandNode::Register(reg) = self {
            if let Some(info) = config::info_of_register(&reg.name) {
                return info.kind == RegisterKind::GeneralPurpose;
            }
        }
        false
    }

    fn is_sreg(&self) -> bool {
        if let OperandNode::Register(reg) = self {
            if let Some(info) = config::info_of_register(&reg.name) {
                return info.kind == RegisterKind::Segment;
            }
        }
        false
    }

    // 是否是以 name 为名字的寄存器(任何类型寄存器)
    fn is_register_of_name(&self, name: &str) -> bool {
        if let OperandNode::Register(reg) = self {
            return &reg.name == name;
        }
        false
    }

    fn is_register_a(&self) -> bool {
        if let OperandNode::Register(reg) = self {
            return match &reg.name[..] {
                "al" | "ax" | "eax" => true,
                _ => false,
            };
        }
        false
    }

    fn is_rm(&self) -> bool {
        self.is_reg() || self.is_mem()
    }

    fn is_imm(&self) -> bool {
        match self {
            OperandNode::Immediate(_) => true,
            _ => false,
        }
    }

    fn is_mem(&self) -> bool {
        match self {
            OperandNode::Memory(..) => true,
            _ => false,
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

    #[test]
    fn test_text_section_visit() {
        let code = r#"
        .section .text
        ret
        ret $3
        int $0x80
        int3
        pop %ax
        pop %ebx
        popw 3
        pop %ds
        mov 3, %ax
        mov %ax, %bx
        movl %eax, 3(%eax)
        movl $2, %eax
        movl $2, 2
        leal 3(%eax), %eax
        "#.trim();
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        let ast = parser.build_ast().unwrap();

        let mut generator = Generator::new();
        println!();
        ast.run_visitor(&mut generator);
    }
}