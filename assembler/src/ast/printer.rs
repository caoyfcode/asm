use std::fmt::Write;

use super::*;

pub struct AstPrinter {
    depth: usize, // 缩进层数
    buf: String,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            depth: 0,
            buf: String::new(),
        }
    }

    pub fn as_str(&self) -> &str {
        self.buf.as_str()
    }

    fn write_indent(&mut self) {
        for _ in 0..self.depth {
            write!(self.buf, "|  ").unwrap();
        }
        write!(self.buf, ">--+ ").unwrap();
    }

    fn inc_depth(&mut self) {self.depth += 1;}
    fn dec_depth(&mut self) {self.depth -= 1;}
}

impl Visitor for AstPrinter {
    fn visit_program(&mut self, node: &ProgramNode) {
        self.write_indent();
        writeln!(self.buf, "program").unwrap();
        self.inc_depth();
        for item in &node.items {
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
        self.dec_depth();
    }

    fn visit_pseudo_section(&mut self, node: &PseudoSectionNode) {
        self.write_indent();
        writeln!(self.buf, "section: {}", node.symbol).unwrap();
    }

    fn visit_pseudo_global(&mut self, node: &PseudoGlobalNode) {
        self.write_indent();
        writeln!(self.buf, "global: {}", node.symbol).unwrap();
    }

    fn visit_pseudo_equ(&mut self, node: &PseudoEquNode) {
        self.write_indent();
        writeln!(self.buf, "equ: {} = {}", node.symbol, node.value).unwrap();
    }

    fn visit_pseudo_fill(&mut self, node: &PseudoFillNode) {
        self.write_indent();
        writeln!(self.buf, "fill: repeat {}, size {}", node.repeat, node.size).unwrap();
        self.inc_depth();
        node.value.accept(self);
        self.dec_depth();
    }

    fn visit_value(&mut self, node: &ValueNode) {
        self.write_indent();
        write!(self.buf, "value: ").unwrap();
        match node {
            ValueNode::Integer(num) => writeln!(self.buf, "{}", num).unwrap(),
            ValueNode::Symbol(sym) => writeln!(self.buf, "{}", sym).unwrap(),
        }
    }

    fn visit_pseudo_integer(&mut self, node: &PseudoIntegerNode) {
        self.write_indent();
        writeln!(self.buf, "{:?} integers:", node.size).unwrap();
        self.inc_depth();
        for value in &node.values {
            value.accept(self);
        }
        self.dec_depth();
    }

    fn visit_pseudo_string(&mut self, node: &PseudoStringNode) {
        self.write_indent();
        write!(self.buf, r#"string: "{}""#, node.content).unwrap();
        match node.zero_end {
            true => writeln!(self.buf, " with a zero end").unwrap(),
            false => writeln!(self.buf, " without a zero end").unwrap(),
        }
    }

    fn visit_pseudo_comm(&mut self, node: &PseudoCommNode) {
        self.write_indent();
        let is_local = match node.is_local {
            true => "local",
            false => "none-local",
        };
        writeln!(self.buf, "{} comm: {}", is_local, node.symbol).unwrap();
        self.inc_depth();
        node.length.accept(self);
        self.dec_depth();
    }

    fn visit_instruction(&mut self, node: &InstructionNode) {
        self.write_indent();
        write!(self.buf, "instruction: mnemonic {}", node.mnemonic).unwrap();
        if let Some(size) = &node.operand_size {
            writeln!(self.buf, ", with a size {:?}", size).unwrap();
        } else {
            writeln!(self.buf).unwrap();
        }

        self.inc_depth();
        for op in &node.operands {
            op.accept(self);
        }
        self.dec_depth();
    }

    fn visit_operand(&mut self, node: &OperandNode) {
        self.write_indent();
        writeln!(self.buf, "operand").unwrap();
        self.inc_depth();
        match node {
            OperandNode::Register(n) => n.accept(self),
            OperandNode::Immediate(n) => n.accept(self),
            OperandNode::Memory(n, None) => n.accept(self),
            OperandNode::Memory(n, Some(reg)) => {
                n.accept(self);
                self.write_indent();
                writeln!(self.buf, "segment register").unwrap();
                self.inc_depth();
                reg.accept(self);
                self.dec_depth();
            },
        }
        self.dec_depth();
    }

    fn visit_register(&mut self, node: &RegisterNode) {
        self.write_indent();
        writeln!(self.buf, "register: {}", node.name).unwrap();
    }

    fn visit_mem(&mut self, node: &MemNode) {
        self.write_indent();
        writeln!(self.buf, "mem").unwrap();
        self.inc_depth();

        if let Some(offset) = &node.offset {
            self.write_indent();
            writeln!(self.buf, "offset").unwrap();
            self.inc_depth();
            offset.accept(self);
            self.dec_depth();
        }

        if let Some(base) = &node.base {
            self.write_indent();
            writeln!(self.buf, "base").unwrap();
            self.inc_depth();
            base.accept(self);
            self.dec_depth();
        }

        if let Some((index, scale)) = &node.index_scale {
            self.write_indent();
            writeln!(self.buf, "index").unwrap();
            self.inc_depth();
            index.accept(self);
            self.dec_depth();
            self.write_indent();
            writeln!(self.buf, "scale {}", scale).unwrap();
        }

        self.dec_depth();
    }

    fn visit_label(&mut self, node: &LabelNode) {
        self.write_indent();
        writeln!(self.buf, "label: {}", node.label).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let program = ProgramNode {
            items: vec![
                ProgramItem::PseudoSection(
                    PseudoSectionNode {symbol: String::from(".text")}
                ),
                ProgramItem::Label(
                    LabelNode { label: String::from("main") }
                ),
                ProgramItem::Instruction(
                    InstructionNode {
                        mnemonic: String::from("mov"),
                        operand_size: Some(Size::DoubleWord),
                        operands: vec![
                            OperandNode::Register(
                                RegisterNode { name: String::from("ecx") }
                            ),
                            OperandNode::Memory(
                                MemNode {
                                    base: Some(
                                        RegisterNode { name: String::from("eax") }
                                    ),
                                    index_scale: Some((
                                        RegisterNode {name: String::from("ebx")},
                                        1
                                    )),
                                    offset: Some(
                                        ValueNode::Integer(4)
                                    )
                                },
                                Some(RegisterNode { name: String::from("ds") }),
                            )
                        ]
                    }
                ),
                ProgramItem::PseudoSection(
                    PseudoSectionNode {symbol: String::from(".data")}
                ),
                ProgramItem::Label(
                    LabelNode { label: String::from("hello") }
                ),
                ProgramItem::PseudoString(
                    PseudoStringNode { zero_end: true, content: String::from("Hello, World!") }
                ),
                ProgramItem::PseudoSection(
                    PseudoSectionNode {symbol: String::from(".bss")}
                ),
                ProgramItem::PseudoComm(
                    PseudoCommNode {
                        is_local: true,
                        symbol: String::from("buf"),
                        length: ValueNode::Integer(8)
                    }
                )
            ]
        };
        let ast = Ast::new(program);
        let mut visitor = AstPrinter::new();
        ast.run_visitor(&mut visitor);
        println!("");
        println!("{}", visitor.as_str());
    }
}