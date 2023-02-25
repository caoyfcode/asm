pub mod printer;

use crate::common::Size;

pub trait Node {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return;
}

pub trait Visitor {
    type Return;
    fn visit_program(&mut self, node: &ProgramNode) -> Self::Return;
    fn visit_pseudo_section(&mut self, node: &PseudoSectionNode) -> Self::Return;
    fn visit_pseudo_global(&mut self, node: &PseudoGlobalNode) -> Self::Return;
    fn visit_pseudo_equ(&mut self, node: &PseudoEquNode) -> Self::Return;
    fn visit_pseudo_fill(&mut self, node: &PseudoFillNode) -> Self::Return;
    fn visit_value(&mut self, node: &ValueNode) -> Self::Return;
    fn visit_pseudo_integer(&mut self, node: &PseudoIntegerNode) -> Self::Return;
    fn visit_pseudo_string(&mut self, node: &PseudoStringNode) -> Self::Return;
    fn visit_pseudo_comm(&mut self, node: &PseudoCommNode) -> Self::Return;
    fn visit_instruction(&mut self, node: &InstructionNode) -> Self::Return;
    fn visit_operand(&mut self, node: &OperandNode) -> Self::Return;
    fn visit_register(&mut self, node: &RegisterNode) -> Self::Return;
    fn visit_mem(&mut self, node: &MemNode) -> Self::Return;
    fn visit_label(&mut self, node: &LabelNode) -> Self::Return;
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

    pub fn run_visitor<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        self.program.accept(visitor)
    }
}

pub struct ProgramNode {
    pub items: Vec<(ProgramItem, usize)>, // usize 为行号, 从 1 开始
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
    Symbol(String),
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
    pub operands: Vec<OperandNode>, // 按照 AT&T 语法顺序
}

pub enum OperandNode {
    Register(RegisterNode),
    Immediate(ValueNode),
    Memory(MemNode, Option<RegisterNode>), // 内存操作数与段寄存器
}

pub struct RegisterNode {
    pub name: String,
}

pub struct MemNode {
    pub displacement: Option<ValueNode>, // 立即数偏移
    pub base: Option<RegisterNode>, // 基址寄存器
    pub index_scale: Option<(RegisterNode, u32)>, // 变址寄存器, scale 必须为 1, 2, 4, 8
}

pub struct LabelNode {
    pub label: String,
}

impl Node for ProgramNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_program(self)
    }
}

impl Node for PseudoSectionNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_pseudo_section(self)
    }
}

impl Node for PseudoGlobalNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_pseudo_global(self)
    }
}

impl Node for PseudoEquNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_pseudo_equ(self)
    }
}

impl Node for PseudoFillNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_pseudo_fill(self)
    }
}

impl Node for ValueNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_value(self)
    }
}

impl Node for PseudoIntegerNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_pseudo_integer(self)
    }
}

impl Node for PseudoStringNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_pseudo_string(self)
    }
}

impl Node for PseudoCommNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_pseudo_comm(self)
    }
}

impl Node for InstructionNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_instruction(self)
    }
}

impl Node for OperandNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_operand(self)
    }
}

impl Node for RegisterNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_register(self)
    }
}

impl Node for MemNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_mem(self)
    }
}

impl Node for LabelNode {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Return {
        visitor.visit_label(self)
    }
}
