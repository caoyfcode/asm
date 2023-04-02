mod table;
mod data;
mod instruction;

use elf::{ProgramSection, Symbol, Relocation};

use crate::ast::{Node, Visitor, ProgramNode, ProgramItem, InstructionNode, LabelNode, PseudoSectionNode, PseudoGlobalNode, PseudoEquNode, PseudoFillNode, PseudoIntegerNode, PseudoStringNode, PseudoLcommNode, ValueNode, OperandNode, RegisterNode, MemNode};
use crate::common::{Size, Error};
use crate::config::{self, OperandEncoding, RegisterKind, InstructionInfo, RegisterInfo};

use table::{SymbolTable, SymbolKind};
use data::Data;
use instruction::Instruction;

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

impl From<Section> for ProgramSection {
    fn from(value: Section) -> Self {
        match value {
            Section::Text => ProgramSection::Text,
            Section::Data => ProgramSection::Data,
            Section::Bss => ProgramSection::Bss,
        }
    }
}

#[derive(Clone, Copy)]
enum Segment {
    Cs,
    Ds,
    Es,
    Fs,
    Gs,
    Ss,
}

impl Segment {
    fn from(register_name: &str) -> Option<Self> {
        match register_name {
            "cs" => Some(Self::Cs),
            "ds" => Some(Self::Ds),
            "es" => Some(Self::Es),
            "fs" => Some(Self::Fs),
            "gs" => Some(Self::Gs),
            "Ss" => Some(Self::Ss),
            _ => None,
        }
    }
}

/// 表示一条指令或数据定义
trait Statement {
    fn length(&self) -> u32;
    fn emit(&self) -> (Vec<u8>, Vec<Relocation>);
}

/// 用于在 Statement 中表示数值或标签或外部符号
enum Value {
    Integer(u32), // 可以确定的数值
    Symbol(String, bool), // 标签或外部符号, (name, is_relative)
}

/// 用于访问语法树生成目标文件
pub struct Generator {
    data_section: Vec<Data>,
    text_section: Vec<Instruction>,
    bss_section_size: u32,
    table: SymbolTable,
    current_section: Section,
    current_offset: u32,
    // 属性
    line: usize, // 行号
}

impl Generator {
    pub fn new() -> Self {
        let mut table = SymbolTable::new();
        table.insert_label(Section::Text.name().to_string(), 0, Section::Text);
        table.insert_label(Section::Data.name().to_string(), 0, Section::Data);
        table.insert_label(Section::Bss.name().to_string(), 0, Section::Bss);
        Self {
            data_section: Vec::new(),
            text_section: Vec::new(),
            bss_section_size: 0,
            table,
            current_section: Section::Text,
            current_offset: 0,
            line: 0,
        }
    }

    pub fn generate_data_section(&self) -> (Vec<u8>, Vec<Relocation>) {
        let mut section: Vec<u8> = Vec::new();
        let mut rel: Vec<Relocation> = Vec::new();
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

    pub fn generate_text_section(&self) -> (Vec<u8>, Vec<Relocation>) {
        let mut section: Vec<u8> = Vec::new();
        let mut rel: Vec<Relocation> = Vec::new();
        for code in &self.text_section {
            let (mut d, mut r) = code.emit();
            let offset = section.len() as u32;
            for info in &mut r {
                info.offset += offset;
            }
            section.append(&mut d);
            rel.append(&mut r);
        }
        self.handle_relative_relocation(Section::Text, &mut section, &mut rel);
        (section, rel)
    }

    pub fn generate_bss_section_size(&self) -> u32 {
        self.bss_section_size
    }

    // 将对于同 section 标签的相对重定位从重定位表删除, 并修改字节数组对应位置
    fn handle_relative_relocation(&self, section: Section, content: &mut Vec<u8>, rel: &mut Vec<Relocation>) {
        let mut removes: Vec<usize> = Vec::new();
        for idx in 0..rel.len() {
            if rel[idx].is_relative {
                if let Some(SymbolKind::Label(val, sec)) = self.table.get_symbol(&rel[idx].symbol) {
                    if section == sec {
                        removes.push(idx);
                        let offset = rel[idx].offset as usize;
                        let addend = content[offset] as u32
                            + ((content[offset + 1] as u32) << 8)
                            + ((content[offset + 2] as u32) << 16)
                            + ((content[offset + 3] as u32) << 24);
                        let rel = val.wrapping_add(addend).wrapping_sub(offset as u32);
                        content[offset] = rel as u8;
                        content[offset + 1] = (rel >> 8) as u8;
                        content[offset + 2] = (rel >> 16) as u8;
                        content[offset + 3] = (rel >> 24) as u8;
                    }
                }
            }
        }
        for rm_idx in (0..removes.len()).rev() {
            rel.remove(removes[rm_idx]);
        }
    }

    pub fn generate_symbol_table(&self) -> Vec<Symbol> {
        // 只留下不是 section 名的符号
        self.table
            .symbols()
            .into_iter()
            .filter(|symbol| Section::from(&symbol.name).is_none())
            .collect()
    }

    /// 若 name 为 equ, 则返回其值, 否则返回重定位信息, 并尝试在符号表插入一个外部符号
    fn value_of(&mut self, name: &String, is_relative: bool) -> Value {
        match self.table.get_symbol(name) {
            Some(SymbolKind::Constant(value)) => Value::Integer(value),
            _ => {
                self.table.insert_external(name.clone());
                Value::Symbol(name.clone(), is_relative)
            },
        }
    }

    // 将 ValueNode 转为 Value, 只有在为整数或 equ 时才返回值, 否则返回重定位信息, 并尝试在符号表插入一个外部符号
    fn value_of_node(&mut self, node: &ValueNode, is_relative: bool) -> Value {
        match node {
            ValueNode::Integer(value) => Value::Integer(*value),
            ValueNode::Symbol(name) => self.value_of(name, is_relative),
        }
    }
}

/// 访问语法树, 从语法树建立符号表, 生成 data section, bss section 与 text section,
/// 但是标签不替换成值, 仅仅把 equ 替换成值
impl Visitor for Generator {
    type Return = Result<(), Error>;

    fn visit_program(&mut self, node: &ProgramNode) -> Self::Return {
        for (item, line) in &node.items {
            self.line = *line;
            match item {
                ProgramItem::PseudoSection(node) => node.accept(self)?,
                ProgramItem::PseudoGlobal(node) => node.accept(self)?,
                ProgramItem::PseudoEqu(node) => node.accept(self)?,
                ProgramItem::PseudoFill(node) => node.accept(self)?,
                ProgramItem::PseudoZero(node) => node.accept(self)?,
                ProgramItem::PseudoInteger(node) => node.accept(self)?,
                ProgramItem::PseudoString(node) => node.accept(self)?,
                ProgramItem::PseudoLcomm(node) => node.accept(self)?,
                ProgramItem::Instruction(node) => node.accept(self)?,
                ProgramItem::Label(node) => node.accept(self)?,
            }
        }
        Ok(())
    }

    fn visit_pseudo_section(&mut self, node: &PseudoSectionNode) -> Self::Return {
        self.current_section = match Section::from(&node.symbol) {
            Some(section) => section,
            None => return Err(Error::UnsupportedSection(self.line, node.symbol.clone())),
        };

        self.current_offset = match self.current_section {
            Section::Text => {
                self.text_section.iter()
                    .map(|code| code.length())
                    .sum()
            }
            Section::Data => {
                self.data_section.iter()
                    .map(|code| code.length())
                    .sum()
            }
            Section::Bss => self.bss_section_size
        };
        Ok(())
    }

    fn visit_pseudo_global(&mut self, node: &PseudoGlobalNode) -> Self::Return {
        if !self.table.set_global(node.symbol.clone()) {
            Err(Error::DefineSymbolFail(self.line, node.symbol.clone(), String::from("global symbol")))
        } else {
            Ok(())
        }
    }

    fn visit_pseudo_equ(&mut self, node: &PseudoEquNode) -> Self::Return {
        if !self.table.insert_constant(node.symbol.clone(), node.value) {
            Err(Error::DefineSymbolFail(self.line, node.symbol.clone(), String::from("constant")))
        } else {
            Ok(())
        }
    }

    fn visit_pseudo_fill(&mut self, node: &PseudoFillNode) -> Self::Return {
        if self.current_section != Section::Data {
            return Err(Error::NotInRightSection(self.line, Section::Data.name().to_string()))
        }
        let repeat = match self.value_of_node(&node.repeat, false) {
            Value::Integer(val) => val,
            Value::Symbol(name, _) => return Err(Error::UseWrongTypeSymbol(self.line, name, String::from("constant"))),
        };
        let size = match self.value_of_node(&node.size, false) {
            Value::Integer(val) => val,
            Value::Symbol(name, _) => return Err(Error::UseWrongTypeSymbol(self.line, name, String::from("constant"))),
        };
        let value = match self.value_of_node(&node.value, false) {
            Value::Integer(val) => val,
            Value::Symbol(name, _) => return Err(Error::UseWrongTypeSymbol(self.line, name, String::from("constant"))),
        };
        if size > 4 {
            println!("{}: warn: size is {} > 4, use as 4", self.line, size);
        }
        let data = Data::new_fill(repeat, size, value);
        self.current_offset += data.length();
        self.data_section.push(data);
        Ok(())
    }

    fn visit_pseudo_zero(&mut self, node: &crate::ast::PseudoZeroNode) -> Self::Return {
        match self.current_section {
            Section::Data => {
                let size = match self.value_of_node(&node.size, false) {
                    Value::Integer(val) => val,
                    Value::Symbol(name, _) => return Err(Error::UseWrongTypeSymbol(self.line, name, String::from("constant"))),
                };
                let data = Data::new_fill(size, 1, 0);
                self.current_offset += data.length();
                self.data_section.push(data);
                Ok(())
            },
            Section::Bss => {
                let size = match self.value_of_node(&node.size, false) {
                    Value::Integer(val) => val,
                    Value::Symbol(name, _) => return Err(Error::UseWrongTypeSymbol(self.line, name, String::from("constant"))),
                };
                self.bss_section_size += size;
                self.current_offset += size;
                Ok(())
            },
            Section::Text => Err(Error::NotInRightSection(self.line, ".data or .bss".to_string())),
        }
    }

    fn visit_value(&mut self, _node: &ValueNode) -> Self::Return {
        panic!("shouldn't visit ValueNode")
    }

    fn visit_pseudo_integer(&mut self, node: &PseudoIntegerNode) -> Self::Return {
        if self.current_section != Section::Data {
            return Err(Error::NotInRightSection(self.line, Section::Data.name().to_string()));
        }
        let values: Vec<Value> = node.values
            .iter()
            .map(|node| self.value_of_node(node, false))
            .collect();
        let data = Data::new(node.size, values);
        self.current_offset += data.length();
        self.data_section.push(data);
        Ok(())
    }

    fn visit_pseudo_string(&mut self, node: &PseudoStringNode) -> Self::Return {
        if self.current_section != Section::Data {
            return Err(Error::NotInRightSection(self.line, Section::Data.name().to_string()));
        }
        let mut values: Vec<Value> = node.content
            .bytes()
            .map(|val| Value::Integer(val as u32))
            .collect();
        if node.zero_end {
            values.push(Value::Integer(0));
        }
        let data = Data::new(Size::Byte, values);
        self.current_offset += data.length();
        self.data_section.push(data);
        Ok(())
    }

    fn visit_pseudo_lcomm(&mut self, node: &PseudoLcommNode) -> Self::Return {
        if self.current_section != Section::Bss {
            return Err(Error::NotInRightSection(self.line, Section::Bss.name().to_string()));
        }
        let length = match self.value_of_node(&node.length, false) {
            Value::Integer(val) => val,
            Value::Symbol(name, _) => return Err(Error::UseWrongTypeSymbol(self.line, name, String::from("constant"))),
        };
        if !self.table.insert_label(node.symbol.clone(), self.current_offset, Section::Bss) {
            return Err(Error::DefineSymbolFail(self.line, node.symbol.clone(), String::from("local common symbol")));
        }
        self.bss_section_size += length;
        self.current_offset += length;
        Ok(())
    }

    fn visit_instruction(&mut self, node: &InstructionNode) -> Self::Return {
        if self.current_section != Section::Text {
            return Err(Error::NotInRightSection(self.line, Section::Text.name().to_string()));
        }
        let infos = config::infos_of_instruction(&node.mnemonic).unwrap(); // 助记符在词法分析时判断
        for info in infos {
            if let Some(inst) = self.make_instruction(info, node.operand_size, &node.operands) {
                self.current_offset += inst.length();
                self.text_section.push(inst);
                return Ok(());
            }
        }
        Err(Error::InvalidOperands(self.line, node.mnemonic.clone()))
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
        if !self.table.insert_label(node.label.clone(), self.current_offset, self.current_section) {
            Err(Error::DefineSymbolFail(self.line, node.label.clone(), String::from("label")))
        } else {
            Ok(())
        }
    }

}

/// 表示一个内存操作数
struct MemOperand {
    disp: Option<Value>,
    base: Option<u8>,
    index_scale: Option<(u8, u8)>,
    seg: Option<Segment>,
}

impl Generator {
    // 检查 info 是否能够匹配 size(助记符后缀) 与 operands(at&t形式操作数), 能则生成一条指令
    fn make_instruction(&mut self, info: &InstructionInfo, size: Option<Size>, operands: &Vec<OperandNode>) -> Option<Instruction> {
        // 存在助记符后缀时, 先判断助记符后缀
        if let Some(size) = size {
            if size != info.operand_size {
                return None;
            }
        }
        // 此时要么不存在助记符后缀, 要么后缀与 info.operand_size 相同
        match info.operand_encoding {
            OperandEncoding::Zero => {
                if operands.len() != 0 {
                    return None;
                }
                if size.is_none() && !info.is_default {
                    return None;
                }
                Instruction::builder()
                    .opcode(&info.opcode)
                    .operand_size_override(info.operand_size == Size::Word) // 如 pusha 需要前缀
                    .build()
            },
            OperandEncoding::Opcode => {
                if operands.len() != 1 || !operands[0].is_greg() {
                    return None;
                }
                let reg_info = operands[0].get_register_info().unwrap();
                if reg_info.size != info.operand_size {
                    return None;
                }
                Instruction::builder()
                    .opcode(&info.opcode)
                    .opcode_rd(reg_info.code)
                    .operand_size_override(info.operand_size == Size::Word)
                    .build()
            },
            OperandEncoding::ImpliedSreg(name) => {
                if operands.len() != 1 || !operands[0].is_register_of_name(name) {
                    return None;
                }
                Instruction::builder()
                    .opcode(&info.opcode)
                    .build()
            },
            OperandEncoding::Rm | OperandEncoding::Mem => {
                if operands.len() != 1 || !operands[0].is_rm() {
                    return None;
                }
                if let OperandEncoding::Mem = info.operand_encoding {
                    if !operands[0].is_mem() {
                        return None;
                    }
                }
                let builder = Instruction::builder()
                    .opcode(&info.opcode)
                    .modrm_reg_opcode(info.modrm_opcode.unwrap())
                    .operand_size_override(info.operand_size == Size::Word);

                if operands[0].is_greg() { // reg
                    let reg_info = operands[0].get_register_info().unwrap();
                    if reg_info.size != info.operand_size {
                        return None;
                    }
                    builder.modrm_rm_r(reg_info.code)
                        .build()
                } else { // mem
                    if size.is_none() && !info.is_default {
                        return None;
                    }
                    let mem = self.get_mem_operand(&operands[0])?;
                    builder.modrm_rm_m(mem.disp, mem.base, mem.index_scale)
                        .segment_override(mem.seg)
                        .build()
                }
            },
            OperandEncoding::Imm(need_prefix) => {
                if operands.len() != 1 || !operands[0].is_imm() {
                    return None;
                }
                if size.is_none() && !info.is_default {
                    return None;
                }
                let value = self.get_imm_operand(&operands[0], false)?;
                Instruction::builder()
                    .opcode(&info.opcode)
                    .immediate(value, info.operand_size)
                    .operand_size_override(info.operand_size == Size::Word && need_prefix) // imm 有不需要前缀的特例
                    .build()
            },
            OperandEncoding::Rel => { // 暂时先不允许操作数直接为数值, 且暂不支持 instruction relaxation, 故其实只有 rel32
                if operands.len() != 1 || !operands[0].is_imm() {
                    return None;
                }
                if !size.is_none() { // jmp/call rel 必须没有后缀, 大小由操作数决定
                    return None;
                }
                let value = self.get_imm_operand(&operands[0], true)?;
                let size = match &value {
                    Value::Integer(_) => return None, // 暂时不支持非标签
                    Value::Symbol(_, _) => Size::DoubleWord,
                };
                if size != info.operand_size {
                    return None;
                }
                Instruction::builder()
                    .opcode(&info.opcode)
                    .immediate(value, info.operand_size)
                    .operand_size_override(info.operand_size == Size::Word)
                    .build()
            },
            OperandEncoding::RegRm | OperandEncoding::RmReg | OperandEncoding::MemReg => {
                let (reg_index, rm_index) = if let OperandEncoding::RegRm = info.operand_encoding {
                    (0usize, 1usize)
                } else {
                    (1usize, 0usize)
                };
                if operands.len() != 2 || !operands[reg_index].is_greg() || !operands[rm_index].is_rm() {
                    return None;
                }
                if let OperandEncoding::MemReg = info.operand_encoding {
                    if !operands[rm_index].is_mem() {
                        return None;
                    }
                }
                let reg = operands[reg_index].get_register_info().unwrap();
                if reg.size != info.operand_size {
                    return None;
                }
                let builder = Instruction::builder()
                    .opcode(&info.opcode)
                    .modrm_reg_opcode(reg.code)
                    .operand_size_override(info.operand_size == Size::Word);
                if operands[rm_index].is_greg() { // r
                    let reg_info = operands[rm_index].get_register_info().unwrap();
                    if reg_info.size != info.operand_size {
                        return None;
                    }
                    builder.modrm_rm_r(reg_info.code)
                        .build()
                } else { // m
                    let mem = self.get_mem_operand(&operands[rm_index])?;
                    builder.modrm_rm_m(mem.disp, mem.base, mem.index_scale)
                        .segment_override(mem.seg)
                        .build()
                }
            },
            OperandEncoding::MoffsA | OperandEncoding::AMoffs => {
                let (moffs_index, a_index) = if let OperandEncoding::MoffsA = info.operand_encoding {
                    (0usize, 1usize)
                } else {
                    (1usize, 0usize)
                };
                if operands.len() != 2 || !operands[moffs_index].is_mem() || !operands[a_index].is_register_a() {
                    return None;
                }
                let reg_a = operands[a_index].get_register_info().unwrap();
                if reg_a.size != info.operand_size {
                    return None;
                }
                let mem = self.get_mem_operand(&operands[moffs_index])?;
                if mem.disp.is_none() || mem.base.is_some() || mem.index_scale.is_some() || mem.seg.is_some() {
                    return None;
                }
                Instruction::builder()
                    .opcode(&info.opcode)
                    .immediate(mem.disp.unwrap(), Size::DoubleWord) // 本应是 opcode 后接着 disp32, 这样能达到同样效果
                    .operand_size_override(info.operand_size == Size::Word)
                    .build()
            },
            OperandEncoding::ImmOpcode => {
                if operands.len() != 2 || !operands[0].is_imm() || !operands[1].is_greg() {
                    return None;
                }
                let reg_info = operands[1].get_register_info().unwrap();
                if reg_info.size != info.operand_size {
                    return None;
                }
                let imm = self.get_imm_operand(&operands[0], false)?;
                Instruction::builder()
                    .opcode(&info.opcode)
                    .opcode_rd(reg_info.code)
                    .immediate(imm, info.operand_size)
                    .operand_size_override(info.operand_size == Size::Word)
                    .build()
            },
            OperandEncoding::ImmRm => {
                if operands.len() != 2 || !operands[0].is_imm() || !operands[1].is_rm() {
                    return None;
                }
                let imm = self.get_imm_operand(&operands[0], false)?;
                if operands[1].is_greg() { // r
                    let reg_info = operands[1].get_register_info().unwrap();
                    if reg_info.size != info.operand_size {
                        return None;
                    }
                    Instruction::builder()
                        .opcode(&info.opcode)
                        .immediate(imm, info.operand_size)
                        .modrm_reg_opcode(info.modrm_opcode.unwrap())
                        .modrm_rm_r(reg_info.code)
                        .operand_size_override(info.operand_size == Size::Word)
                        .build()
                } else { // m
                    if size.is_none() && !info.is_default {
                        return None;
                    }
                    let mem = self.get_mem_operand(&operands[1])?;
                    Instruction::builder()
                        .opcode(&info.opcode)
                        .immediate(imm, info.operand_size)
                        .modrm_reg_opcode(info.modrm_opcode.unwrap())
                        .modrm_rm_m(mem.disp, mem.base, mem.index_scale)
                        .operand_size_override(info.operand_size == Size::Word)
                        .segment_override(mem.seg)
                        .build()
                }
            },
            OperandEncoding::ImmA => {
                if operands.len() != 2 || !operands[0].is_imm() || !operands[1].is_register_a() {
                    return None;
                }
                let reg_info = operands[1].get_register_info().unwrap();
                if reg_info.size != info.operand_size {
                    return None;
                }
                let imm = self.get_imm_operand(&operands[0], false)?;
                Instruction::builder()
                    .opcode(&info.opcode)
                    .immediate(imm, info.operand_size)
                    .operand_size_override(info.operand_size == Size::Word)
                    .build()
            },
            OperandEncoding::SregRm | OperandEncoding::RmSreg => {
                let (sreg_index, rm_index) = if let OperandEncoding::SregRm = info.operand_encoding {
                    (0usize, 1usize)
                } else {
                    (1usize, 0usize)
                };
                if operands.len() != 2 || !operands[sreg_index].is_sreg() || !operands[rm_index].is_rm() {
                    return None;
                }
                let sreg = operands[sreg_index].get_register_info().unwrap();
                let builder = Instruction::builder()
                    .opcode(&info.opcode)
                    .modrm_reg_opcode(sreg.code);
                if operands[rm_index].is_greg() { // r
                    let reg_info = operands[rm_index].get_register_info().unwrap();
                    if reg_info.size != info.operand_size {
                        return None;
                    }
                    builder.modrm_rm_r(reg_info.code)
                        .operand_size_override(info.operand_size == Size::Word)
                        .build()
                } else { // m
                    if size.is_none() && !info.is_default { // SregRm 可以是 16 或 32, 因而无法确定大小
                        return None;
                    }
                    let mem = self.get_mem_operand(&operands[rm_index])?;
                    builder.modrm_rm_m(mem.disp, mem.base, mem.index_scale)
                        .segment_override(mem.seg)
                        .operand_size_override(info.operand_size == Size::Word && rm_index == 1) // mem, sreg 不需要前缀, 因为只有 m16
                        .build()
                }
            },
            OperandEncoding::ImmRmReg => {
                if operands.len() != 3 || !operands[0].is_imm() || !operands[1].is_rm() || !operands[2].is_greg() {
                    return None;
                }
                let reg_info = operands[2].get_register_info().unwrap();
                if reg_info.size != info.operand_size {
                    return None;
                }
                let imm = self.get_imm_operand(&operands[0], false)?;
                let builder = Instruction::builder()
                    .opcode(&info.opcode)
                    .modrm_reg_opcode(reg_info.code)
                    .immediate(imm, info.operand_size)
                    .operand_size_override(info.operand_size == Size::Word);
                if operands[1].is_greg() { // r
                    let reg_info = operands[1].get_register_info().unwrap();
                    if reg_info.size != info.operand_size {
                        return None;
                    }
                    builder.modrm_rm_r(reg_info.code)
                        .build()
                } else { // m
                    let mem = self.get_mem_operand(&operands[1])?;
                    builder.modrm_rm_m(mem.disp, mem.base, mem.index_scale)
                        .segment_override(mem.seg)
                        .build()
                }
            }
        }
    }

    // 将 OperandNode 转为 Value, 失败则返回 None
    fn get_imm_operand(&mut self, node: &OperandNode, is_relative: bool) -> Option<Value> {
        let value_node = match node {
            OperandNode::Immediate(v) => v,
            _ => return None,
        };
        Some(self.value_of_node(value_node, is_relative))
    }

    // 将 OperandNode 转为 MemOperand, 失败则返回 None
    fn get_mem_operand(&mut self, node: &OperandNode) -> Option<MemOperand> {
        let (mem, seg) = match node {
            OperandNode::Memory(mem, seg) => (mem, seg),
            _ => return None,
        };
        let seg = match seg {
            Some(node) => {
                let reg_info = config::info_of_register(&node.name)?;
                if reg_info.kind != RegisterKind::Segment {
                    return None;
                }
                Some(Segment::from(&node.name).unwrap())
            }
            None => None,
        };
        let disp = match &mem.displacement {
            Some(node) => Some(self.value_of_node(&node, false)),
            None => None,
        };
        let base = match &mem.base {
            Some(reg) => {
                let reg_info = config::info_of_register(&reg.name)?;
                if reg_info.size != Size::DoubleWord || reg_info.kind != RegisterKind::GeneralPurpose {
                    return None;
                }
                Some(reg_info.code)
            }
            None => None,
        };
        let index_scale = match &mem.index_scale {
            Some((index, scale)) => {
                if index.name == "esp" {
                    return None;
                }
                let reg_info = config::info_of_register(&index.name)?;
                if reg_info.size != Size::DoubleWord || reg_info.kind != RegisterKind::GeneralPurpose {
                    return None;
                }
                let scale = *scale;
                if scale != 1 && scale !=2 && scale != 4 && scale != 8 {
                    return None;
                }
                Some((reg_info.code, scale as u8))
            }
            None => None,
        };
        if disp.is_none() && base.is_none() && index_scale.is_none() {
            return None;
        }
        Some(MemOperand { disp, base, index_scale, seg })
    }
}

impl OperandNode {
    fn get_register_info(&self) -> Option<&'static RegisterInfo> {
        if let OperandNode::Register(reg) = self {
            config::info_of_register(&reg.name)
        } else {
            None
        }
    }

    /// 是否是通用寄存器
    fn is_greg(&self) -> bool {
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
        self.is_greg() || self.is_mem()
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
        assemble_and_print_msg(
            r#"
            .section .data
            arr:
                .int 1, 3, 5, 7
            byte_arr:
                .byte 1, 3, 5, 7
            str_addr:
                .int str
            str:
                .string "Hello, world!"
            ones:
                .fill 3, 3, 1
            zeros:
                .zero 10
            ptr:
                .int extern_addr
            .section .text
                .global main
            main:
            "#.trim()
        );
    }

    #[test]
    fn test_text_section() {
        assemble_and_print_msg(
            r#"
            .section .data
            hello:
                .string "Hello, World!"
            .section .text
            main:
                push %ebp
                mov %esp, %ebp
                pushl $hello
                call printf
                jmp end
                nop
            end:
                call hehe
                mov %ebp, %esp
                pop %ebp
                ret
            hehe:
                ret
            "#.trim()
        );
    }

    #[test]
    fn test_bss_section() {
        assemble_and_print_msg(
            r#"
                .section .data
                msg:
                    .string "value is %d"
                .section .text
                main:
                    push %ebp
                    mov %esp, %ebp
                    addl $1, value
                    pushl value
                    pushl $msg
                    call printf
                    mov %ebp, %esp
                    pop %ebp
                    ret
                .section .bss
                    .lcomm value, 4
                value2:
                    .zero 10
            "#.trim()
        );
    }

    fn assemble_and_print_msg(src_code: &str) {
        let cursor = Cursor::new(src_code);
        let mut parser = Parser::new(cursor);
        let ast = parser.build_ast().unwrap();

        let mut generator = Generator::new();
        ast.run_visitor(&mut generator).unwrap();

        let (data_sec, data_rel) = generator.generate_data_section();
        let (text_sec, text_rel) = generator.generate_text_section();
        let bss_size = generator.generate_bss_section_size();
        let symbols = generator.generate_symbol_table();

        println!();
        println!("symbol table is:");
        println!("offset\tsection\tglobal\tname");
        for Symbol { name, value, section, is_global } in symbols {
            let section = match section {
                ProgramSection::Undef => "",
                ProgramSection::Text => ".text",
                ProgramSection::Data => ".data",
                ProgramSection::Bss => ".bss",
            };
            println!("0x{:x}\t{}\t{}\t{}", value, section, is_global, name);
        }

        println!();
        println!(".data section is:");
        let mut len: usize = 0;
        let mut splits: Vec<usize> = Vec::new();
        for inst in &generator.data_section {
            len += inst.length() as usize;
            splits.push(len);
        }
        print_hex(&data_sec, &splits);

        println!();
        println!(".text section is:");
        let mut len: usize = 0;
        let mut splits: Vec<usize> = Vec::new();
        for inst in &generator.text_section {
            len += inst.length() as usize;
            splits.push(len);
        }
        print_hex(&text_sec, &splits);

        println!();
        println!(".bss section size is: {bss_size}");

        println!();
        println!(".data relocation table is:");
        println!("offset\tname\tis_relative");
        for rel in data_rel {
            println!("0x{:x}\t{}\t{}", rel.offset, rel.symbol, rel.is_relative);
        }

        println!();
        println!(".text relocation table is:");
        println!("offset\tname\tis_relative");
        for rel in text_rel {
            println!("0x{:x}\t{}\t{}", rel.offset, rel.symbol, rel.is_relative);
        }
    }

    // 将 datas 显示为 16 进制形式, 且在 splits 处分割, 每部分占一行
    fn print_hex(datas: &[u8], splits: &[usize]) {
        let mut cur = 0usize;
        print!("{:0>4x}: ", cur);
        for split_next in splits {
            while cur < *split_next {
                print!("{:02x} ", datas[cur]);
                cur += 1;
            }
            println!();
            print!("{:0>4x}: ", cur);
        }
        println!();
    }

}

#[cfg(test)]
mod tests_inst_gen {
    use std::io::Cursor;

    use super::*;
    use crate::parser::Parser;

        // inst 为用换行分割的指令指令, 返回指令生成的字节数值
    fn assemble_instruction(inst: &str) -> Vec<u8> {
        let src_code = format!(".section .text\n{}\n", inst);
        let cursor = Cursor::new(src_code);
        let mut parser = Parser::new(cursor);
        let ast = parser.build_ast().unwrap();

        // let mut printer = crate::ast::printer::AstPrinter::new();
        // ast.run_visitor(&mut printer);
        // println!();
        // println!("{}", printer.as_str());

        let mut generator = Generator::new();
        ast.run_visitor(&mut generator).unwrap();
        let (text, _) = generator.generate_text_section();
        text
    }

    #[test]
    fn test_zero_ret() {
        let code = assemble_instruction("ret");
        assert_eq!(&code, &[0xc3]);
    }

    #[test]
    fn test_opcode_pop() {
        let code = assemble_instruction(
            r#"
                pop %eax
                pop %cx
                popw %dx
            "#.trim()
        );
        assert_eq!(
            &code,
            &[
                0x58, // pop %eax
                0x66, 0x59, // pop %cx
                0x66, 0x5a, // popw %dx
            ]
        );
    }

    #[test]
    fn test_implied_sreg_pop() {
        let code = assemble_instruction(
            "pop %ds"
        );
        assert_eq!(&code, &[0x1f]);
    }

    #[test]
    fn test_rm_push_and_mem_pop() {
        let code = assemble_instruction(
            r#"
                pushl 3(%eax)
                popw 3
            "#.trim()
        );
        assert_eq!(
            &code,
            &[
                0xff, 0x70, 0x03, // pushl 3(%eax)
                0x66, 0x8f, 0x05, 0x03, 0x00, 0x00, 0x00 // popw 3
            ]
        );
    }

    #[test]
    fn test_jmp_rm_jmp() {
        let code = assemble_instruction(
            r#"
                jmp *%eax
                jmp *%ax
                jmp *3
            "#.trim()
        );
        assert_eq!(
            &code,
            &[
                0xff, 0xe0, // jmp *%eax
                0x66, 0xff, 0xe0, // jmp *eax
                0xff, 0x25, 0x03, 0x00, 0x00, 0x00, // jmp *3
            ]
        );
    }

    #[test]
    fn test_imm8_int() {
        let code = assemble_instruction(
            "int $0x80"
        );
        assert_eq!(&code, &[0xcd, 0x80]);
    }

    #[test]
    fn test_imm8_push() {
        let code = assemble_instruction(
            "pushb $0x03"
        );
        assert_eq!(&code, &[0x6a, 0x03]);
    }

    #[test]
    fn test_imm16_ret() {
        let code = assemble_instruction(
            "ret $0x03"
        );
        assert_eq!(&code, &[0xc2, 0x03, 0x00])
    }

    #[test]
    fn test_imm16_push() {
        let code = assemble_instruction(
            "pushw $0x03"
        );
        assert_eq!(&code, &[0x66, 0x68, 0x03, 0x00]);
    }

    #[test]
    fn test_rel32_jmp() {
        let code = assemble_instruction(
            "jmp label"
        );
        assert_eq!(&code, &[0xe9, 0xfc, 0xff, 0xff, 0xff]); // addend = -4
    }

    #[test]
    fn test_reg_rm_mov() {
        let code = assemble_instruction(
            "mov %eax, (%eax)"
        );
        assert_eq!(&code, &[0x89, 0x00]);
    }

    #[test]
    fn test_rm_reg_mov() {
        let code = assemble_instruction(
            "mov (%eax), %eax"
        );
        assert_eq!(&code, &[0x8b, 0x00]);
    }

    #[test]
    fn test_moffs_a_mov() {
        let code = assemble_instruction(
            "mov 0x1010, %eax"
        );
        assert_eq!(&code, &[0xa1, 0x10, 0x10, 0x00, 0x00]);
    }

    #[test]
    fn test_a_moffs_mov() {
        let code = assemble_instruction(
            "mov %eax, 0x1010"
        );
        assert_eq!(&code, &[0xa3, 0x10, 0x10, 0x00, 0x00]);
    }

    #[test]
    fn test_imm_opcode_mov() {
        let code = assemble_instruction(
            "mov $1, %ecx"
        );
        assert_eq!(&code, &[0xb9, 0x01, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_imm_rm_mov() {
        let code = assemble_instruction(
            "movb $1, (%eax)"
        );
        assert_eq!(&code, &[0xc6, 0x00, 0x01]);
    }

    #[test]
    fn test_mem_reg_lea() {
        let code = assemble_instruction(
            "lea (%eax), %eax"
        );
        assert_eq!(&code, &[0x8d, 0x00]);
    }

    #[test]
    fn test_imm_a_add() {
        let code = assemble_instruction(
            "add $1, %al"
        );
        assert_eq!(&code, &[0x04, 0x01]);
    }

    #[test]
    fn test_sreg_rm_mov() {
        let code = assemble_instruction(
            "movw %es, (%eax)"
        );
        assert_eq!(&code, &[0x66, 0x8c, 0x00])
    }

    #[test]
    fn test_rm_sreg_mov() {
        let code = assemble_instruction(
            "movw (%eax), %es"
        );
        assert_eq!(&code, &[0x8e, 0x00]);
    }

    #[test]
    fn test_imm_rm_reg_imul() {
        let code = assemble_instruction(
            "imul $2, %ebx, %eax"
        );
        assert_eq!(&code, &[0x69, 0xc3, 0x02, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_imm_rm_reg_imul16() {
        let code = assemble_instruction(
            "imul $4, %ax, %ax"
        );
        assert_eq!(&code, &[0x66, 0x69, 0xc0, 0x04, 0x00]);
    }

    // 以下为特殊指令的测试

    #[test]
    fn test_pusha() {
        let code = assemble_instruction(
            "pusha\npushal\npushaw"
        );
        assert_eq!(
            &code,
            &[
                0x60, // pusha
                0x60, // pushal
                0x66, 0x60, // pushaw
            ]
        );
    }

    #[test]
    fn test_jcc() {
        let code = assemble_instruction(
            r#"
                ja label
                jc label
                label:
            "#.trim()
        );
        assert_eq!(
            &code,
            &[
                0x0f, 0x87, 0x06, 0x00, 0x00, 0x00, // ja label
                0x0f, 0x82, 0x00, 0x00, 0x00, 0x00, // jb label ; (jc label)
            ]
        );
    }
}

#[cfg(test)]
mod test_errors {
    use std::io::Cursor;

    use crate::parser::Parser;

    use super::Generator;

    fn run_assembler(src_code: &str) {
        let cursor = Cursor::new(src_code);
        let mut parser = Parser::new(cursor);
        let ast = parser.build_ast().unwrap();

        // let mut printer = crate::ast::printer::AstPrinter::new();
        // ast.run_visitor(&mut printer);
        // println!();
        // println!("{}", printer.as_str());

        let mut generator = Generator::new();
        if let Err(e) = ast.run_visitor(&mut generator) {
            panic!("{}", e);
        }
    }

    #[test]
    #[should_panic = "1: error: unsupported section \"hehe\""]
    fn test_unsupported_section() {
        run_assembler(".section hehe");
    }

    #[test]
    #[should_panic = "3: error: \"a\" can't be a new label"]
    fn test_define_symbol_fail() {
        run_assembler(
            r#"
            .equ a, 1
            .section .data
            a:
            "#.trim()
        );
    }

    #[test]
    #[should_panic = "3: error: \"a\" is not a constant"]
    fn test_use_wrong_type_symbol() {
        run_assembler(
            r#"
            .section .data
            a:
            .fill 3, 4, a
            "#.trim()
        );
    }

    #[test]
    #[should_panic = "2: error: not in .data section"]
    fn test_not_in_right_section() {
        run_assembler(
            r#"
            .section .text
            .int 3
            "#.trim()
        );
    }

    #[test]
    #[should_panic = "2: error: operands of \"push\" are invalid"]
    fn test_invalid_operands() {
        run_assembler(
            r#"
            .section .text
            push 1, 2, 3
            "#.trim()
        );
    }

    #[test]
    #[should_panic = "2: error: \".text\" can't be a new label"]
    fn test_section_name_as_label() {
        run_assembler(
            r#"
            .section .text
            .text:
                ret
            "#.trim()
        );
    }
}