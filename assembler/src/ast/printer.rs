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
    type Return = ();
    fn visit_program(&mut self, node: &ProgramNode) {
        self.write_indent();
        writeln!(self.buf, "program").unwrap();
        self.inc_depth();
        for (item, _) in &node.items {
            match item {
                ProgramItem::PseudoSection(node) => node.accept(self),
                ProgramItem::PseudoGlobal(node) => node.accept(self),
                ProgramItem::PseudoEqu(node) => node.accept(self),
                ProgramItem::PseudoFill(node) => node.accept(self),
                ProgramItem::PseudoZero(node) => node.accept(self),
                ProgramItem::PseudoInteger(node) => node.accept(self),
                ProgramItem::PseudoString(node) => node.accept(self),
                ProgramItem::PseudoLcomm(node) => node.accept(self),
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
        writeln!(self.buf, "fill:").unwrap();

        self.inc_depth();

        self.write_indent();
        writeln!(self.buf, "repeat").unwrap();
        self.inc_depth();
        node.repeat.accept(self);
        self.dec_depth();

        self.write_indent();
        writeln!(self.buf, "size").unwrap();
        self.inc_depth();
        node.size.accept(self);
        self.dec_depth();

        self.write_indent();
        writeln!(self.buf, "value").unwrap();
        self.inc_depth();
        node.value.accept(self);
        self.dec_depth();

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

    fn visit_pseudo_zero(&mut self, node: &PseudoZeroNode) {
        self.write_indent();
        writeln!(self.buf, "fill:").unwrap();

        self.inc_depth();
        node.size.accept(self);
        self.dec_depth();
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

    fn visit_pseudo_lcomm(&mut self, node: &PseudoLcommNode) {
        self.write_indent();
        writeln!(self.buf, "lcomm: {}", node.symbol).unwrap();
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

        if let Some(offset) = &node.displacement {
            self.write_indent();
            writeln!(self.buf, "displacement").unwrap();
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
                (ProgramItem::PseudoSection(
                    PseudoSectionNode {symbol: String::from(".text")}
                ), 1),
                (ProgramItem::Label(
                    LabelNode { label: String::from("main") }
                ), 2),
                (ProgramItem::Instruction(
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
                                    displacement: Some(
                                        ValueNode::Integer(4)
                                    )
                                },
                                Some(RegisterNode { name: String::from("ds") }),
                            )
                        ]
                    }
                ), 3),
                (ProgramItem::PseudoSection(
                    PseudoSectionNode {symbol: String::from(".data")}
                ), 4),
                (ProgramItem::Label(
                    LabelNode { label: String::from("hello") }
                ), 5),
                (ProgramItem::PseudoString(
                    PseudoStringNode { zero_end: true, content: String::from("Hello, World!") }
                ), 6),
                (ProgramItem::PseudoSection(
                    PseudoSectionNode {symbol: String::from(".bss")}
                ), 7),
                (ProgramItem::PseudoLcomm(
                    PseudoLcommNode {
                        symbol: String::from("buf"),
                        length: ValueNode::Integer(8)
                    }
                ), 8)
            ]
        };
        let ast = Ast::new(program);
        let mut visitor = AstPrinter::new();
        ast.run_visitor(&mut visitor);
        println!("");
        println!("{}", visitor.as_str());
    }
}