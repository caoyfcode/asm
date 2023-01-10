pub mod printer;

use crate::common::Size;

pub trait Node {
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

pub trait Visitor {
    fn visit_program(&mut self, node: &ProgramNode);
    fn visit_pseudo_section(&mut self, node: &PseudoSectionNode);
    fn visit_pseudo_global(&mut self, node: &PseudoGlobalNode);
    fn visit_pseudo_equ(&mut self, node: &PseudoEquNode);
    fn visit_pseudo_fill(&mut self, node: &PseudoFillNode);
    fn visit_value(&mut self, node: &ValueNode);
    fn visit_pseudo_integer(&mut self, node: &PseudoIntegerNode);
    fn visit_pseudo_string(&mut self, node: &PseudoStringNode);
    fn visit_pseudo_comm(&mut self, node: &PseudoCommNode);
    fn visit_instruction(&mut self, node: &InstructionNode);
    fn visit_operand(&mut self, node: &OperandNode);
    fn visit_register(&mut self, node: &RegisterNode);
    fn visit_mem(&mut self, node: &MemNode);
    fn visit_label(&mut self, node: &LabelNode);
}

pub struct Ast {
    program: ProgramNode,
}

impl Ast {
    pub fn new(program: ProgramNode) -> Self {
        Self {
            program,
        }
    }

    pub fn run_visitor<V: Visitor>(&self, visitor: &mut V) {
        self.program.accept(visitor);
    }
}

pub struct ProgramNode {
    pub items: Vec<ProgramItem>,
}

pub enum ProgramItem {
    PseudoSection(PseudoSectionNode),
    PseudoGlobal(PseudoGlobalNode),
    PseudoEqu(PseudoEquNode),
    PseudoFill(PseudoFillNode),
    PseudoInteger(PseudoIntegerNode),
    PseudoString(PseudoStringNode),
    PseudoComm(PseudoCommNode),
    Instruction(InstructionNode),
    Label(LabelNode),
}

pub struct PseudoSectionNode {
    pub symbol: String,
}

pub struct PseudoGlobalNode {
    pub symbol: String,
}

pub struct PseudoEquNode {
    pub symbol: String,
    pub value: u32,
}

pub struct PseudoFillNode {
    pub repeat: u32,
    pub size: u32,
    pub value: ValueNode,
}

pub enum ValueNode {
    Integer(u32),
    Symbol(String), // 必须是 equ 定义的
}

pub struct PseudoIntegerNode {
    pub size: Size,
    pub values: Vec<ValueNode>,
}

pub struct PseudoStringNode {
    pub zero_end: bool,
    pub content: String,
}

pub struct PseudoCommNode {
    pub is_local: bool,
    pub symbol: String,
    pub length: ValueNode,
}

pub struct InstructionNode {
    pub mnemonic: String,
    pub operand_size: Option<Size>,
    pub oprands: Vec<OperandNode>, // dest, src1, src2, ...
}

pub enum OperandNode {
    Register(RegisterNode),
    Immediate(ValueNode),
    Memory(MemNode),
}

pub struct RegisterNode {
    pub name: String,
}

pub struct MemNode {
    pub base: Option<RegisterNode>, // 基址寄存器
    pub index_scale: Option<(RegisterNode, u32)>, // 变址寄存器, scale 必须为 1, 2, 4, 8
    pub offset: Option<ValueNode>, // 立即数偏移
}

pub struct LabelNode {
    pub label: String,
}

impl Node for ProgramNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_program(self);
    }
}

impl Node for PseudoSectionNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_pseudo_section(self);
    }
}

impl Node for PseudoGlobalNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_pseudo_global(self);
    }
}

impl Node for PseudoEquNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_pseudo_equ(self);
    }
}

impl Node for PseudoFillNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_pseudo_fill(self);
    }
}

impl Node for ValueNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_value(self);
    }
}

impl Node for PseudoIntegerNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_pseudo_integer(self);
    }
}

impl Node for PseudoStringNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_pseudo_string(self);
    }
}

impl Node for PseudoCommNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_pseudo_comm(self);
    }
}

impl Node for InstructionNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_instruction(self);
    }
}

impl Node for OperandNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_operand(self);
    }
}

impl Node for RegisterNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_register(self);
    }
}

impl Node for MemNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_mem(self);
    }
}

impl Node for LabelNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_label(self)
    }
}
