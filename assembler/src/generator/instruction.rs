use crate::common::Size;

use super::{Statement, RelocationInfo, Value, Segment};

pub(super) struct Instruction {
    prefix: Vec<u8>,
    opcode: Vec<u8>,
    modrm: Option<u8>,
    sib: Option<u8>,
    displacement: Option<(Value, Size)>,
    immediate: Option<(Value, Size)>,
}

impl Instruction {
    pub(super) fn builder() -> InstructionBuilder {
        InstructionBuilder::new()
    }
}

impl Statement for Instruction {
    fn length(&self) -> u32 {
        let mut len = self.prefix.len() as u32 + self.opcode.len() as u32;
        if self.modrm.is_some() {
            len += 1;
        }
        if self.sib.is_some() {
            len += 1;
        }
        if let Some((_, size)) = &self.displacement {
            len += size.length();
        }
        if let Some((_, size)) = self.immediate {
            len += size.length();
        }
        len
    }

    fn emit(&self) -> (Vec<u8>, Vec<RelocationInfo>) {
        let mut text: Vec<u8> = Vec::with_capacity(self.length() as usize);
        let mut rel: Vec<RelocationInfo> = Vec::new();
        text.extend(self.prefix.iter());
        text.extend(self.opcode.iter());
        if let Some(modrm) = self.modrm {
            text.push(modrm);
        }
        if let Some(sib) = self.sib {
            text.push(sib);
        }
        match &self.displacement {
            Some((Value::Integer(val), Size::Byte)) => {
                text.push(*val as u8);
            }
            Some((Value::Integer(_), Size::Word)) => {
                panic!("unsupported displacement size");
            }
            Some((Value::Integer(val), Size::DoubleWord)) => {
                let val = *val;
                text.push(val as u8);
                text.push((val >> 8) as u8);
                text.push((val >> 16) as u8);
                text.push((val >> 24) as u8);
            }
            Some((Value::Symbol(name, is_relative), _)) => {
                let offset = text.len() as u32;
                let addend = if *is_relative {
                    (offset as i64 - self.length() as i64) as u32
                } else {
                    0u32
                };
                text.push(addend as u8);
                text.push((addend >> 8) as u8);
                text.push((addend >> 16) as u8);
                text.push((addend >> 24) as u8);
                rel.push(RelocationInfo { offset, name: name.clone(), is_relative: *is_relative })
            }
            None => (),
        }
        match &self.immediate {
            Some((Value::Integer(val), Size::Byte)) => {
                text.push(*val as u8);
            }
            Some((Value::Integer(val), Size::Word)) => {
                let val = *val;
                text.push(val as u8);
                text.push((val >> 8) as u8);
            }
            Some((Value::Integer(val), Size::DoubleWord)) => {
                let val = *val;
                text.push(val as u8);
                text.push((val >> 8) as u8);
                text.push((val >> 16) as u8);
                text.push((val >> 24) as u8);
            }
            Some((Value::Symbol(name, is_relative), _)) => {
                let offset = text.len() as u32;
                let addend = if *is_relative {
                    (offset as i64 - self.length() as i64) as u32
                } else {
                    0u32
                };
                text.push(addend as u8);
                text.push((addend >> 8) as u8);
                text.push((addend >> 16) as u8);
                text.push((addend >> 24) as u8);
                rel.push(RelocationInfo { offset, name: name.clone(), is_relative: *is_relative })
            }
            None => (),
        }
        (text, rel)
    }
}

pub(super) struct InstructionBuilder {
    segment_override: Option<Segment>,
    operand_size_override: bool,
    opcode: Vec<u8>,
    opcode_rd: Option<u8>,
    modrm: Option<u8>,
    sib: Option<u8>,
    displacement: Option<(Value, Size)>,
    immediate: Option<(Value, Size)>,
}

impl InstructionBuilder {
    fn new() -> Self {
        Self {
            segment_override: None,
            operand_size_override: false,
            opcode: Vec::new(),
            opcode_rd: None,
            modrm: None,
            sib: None,
            displacement: None,
            immediate: None,
        }
    }

    pub(super) fn operand_size_override_if(mut self, has_prefix: bool) -> Self {
        self.operand_size_override = has_prefix;
        self
    }

    pub(super) fn segment_override(mut self, segment: Segment) -> Self {
        self.segment_override = Some(segment);
        self
    }

    pub(super) fn opcode(mut self, opcode: &[u8]) -> Self {
        self.opcode.extend_from_slice(opcode);
        self
    }

    pub(super) fn opcode_rd(mut self, rd: u8) -> Self {
        self.opcode_rd = Some(rd);
        self
    }

    pub(super) fn modrm_reg_opcode(mut self, code: u8) -> Self {
        let modrm = match self.modrm {
            Some(modrm) => modrm,
            _ => 0,
        };
        self.modrm = Some(modrm | ((code & 0b111) << 3));
        self
    }

    pub(super) fn modrm_rm_r(mut self, code: u8) -> Self {
        let modrm = match self.modrm {
            Some(modrm) => modrm,
            _ => 0,
        };
        self.modrm = Some(0b1100_0000 | modrm | (code & 0b111));
        self.sib = None;
        self.displacement = None;
        self
    }

    // 调用时需要保证 index 不为 esp, scale 为 1, 2, 4, 8, 且 base, index 为小于 8, 且三者不能同时为 None
    pub(super) fn modrm_rm_m(mut self, disp: Option<Value>, base: Option<u8>, index_scale: Option<(u8, u8)>) -> Self {
        if let Some(base) = base {
            if base >= 8 {
                panic!("unknown base register");
            }
        }
        if let Some((index, scale)) = index_scale {
            if index >= 8 {
                panic!("unknown index register");
            }
            if index == 4 {
                panic!("%esp shouldn't be index register");
            }
            if scale != 1 && scale != 2 && scale != 4 && scale != 8 {
                panic!("{} can't be scale", scale);
            }
        }
        if disp.is_none() && base.is_none() && index_scale.is_none() {
            panic!("base, index_scale, disp are all none");
        }
        let modrm = match self.modrm {
            Some(modrm) => modrm,
            _ => 0,
        };
        let disp = match (&base, disp) { // %ebp 为 base 时, 默认带 disp
            (Some(5), None) => Some(Value::Integer(0)),
            (_, disp) => disp,
        };
        let disp = match disp { // 为 disp 计算大小
            Some(Value::Integer(val)) => Some((Value::Integer(val), Size::size_of(val))),
            Some(v @ Value::Symbol(..)) => Some((v, Size::DoubleWord)),
            None => None,
        };

        match (base, index_scale, disp) {
            // disp
            (None, None, Some((val, _))) => { // mod = 00, r/m=101, disp32
                self.modrm = Some(0b0 | modrm | 5);
                self.sib = None;
                self.displacement = Some((val, Size::DoubleWord));
            }
            // disp(%base) 且 base 不为 esp(4)
            // (%base) 且 base 不为 esp(4), ebp(5), 其中 (%ebp) 被修改为了 0(%ebp)
            (Some(rm @ (0 | 1 | 2 | 3 | 5 | 6 | 7)), None, disp @ Some(_)) |
            (Some(rm @ (0 | 1 | 2 | 3 | 6 | 7)), None, disp @ None)
            => { // mod=00|01|11, r/m != 100
                self.sib = None;
                match disp {
                    None => { // mod=00, no disp
                        self.modrm = Some(0b0 | modrm | rm);
                        self.displacement = None;
                    }
                    Some((value, Size::Byte)) => { // mod=01, disp8
                        self.modrm = Some(0b0100_0000 | modrm | rm);
                        self.displacement = Some((value, Size::Byte));
                    }
                    Some((value, Size::Word | Size::DoubleWord)) => { // mod=10, disp32
                        self.modrm = Some(0b1000_0000 | modrm | rm);
                        self.displacement = Some((value, Size::DoubleWord));
                    }
                }
            }
            // 以下为带 SIB 字段的
            (base, index_scale, disp) => {
                let mut sib: u8 = 0;
                match base {
                    // disp(%esp), (%esp)
                    // (%base, %index, scale), base 不为 ebp, 因为此时被添加了 disp
                    // disp(%base, %index, scale)
                    Some(base) => {
                        match disp {
                            None => { // mod=00, no disp
                                self.modrm = Some(0 | modrm | 4);
                                self.displacement = None;
                            }
                            Some((value, Size::Byte)) => { // mod=01, disp8
                                self.modrm = Some(0b0100_0000 | modrm | 4);
                                self.displacement = Some((value, Size::Byte));
                            }
                            Some((value, Size::Word | Size::DoubleWord)) => { // mod=10, disp32
                                self.modrm = Some(0b1000_0000 | modrm | 4);
                                self.displacement = Some((value, Size::DoubleWord));
                            }
                        };
                        sib = sib | (base & 0b111);
                    }
                    // (%index, scale), disp(%index, scale)
                    None => {
                        let size = Size::DoubleWord;
                        let disp = match disp {
                            Some((value, _)) => value,
                            _ => Value::Integer(0),
                        };
                        self.modrm = Some(0b00 | modrm | 4);
                        self.displacement = Some((disp, size));
                        sib = sib | 5;
                    }
                }
                if let Some((index, scale)) = index_scale {
                    let ss: u8 = match scale {
                        1 => 0,
                        2 => 1,
                        4 => 2,
                        8 => 3,
                        _ => panic!("scale shouldn't be {}", scale),
                    };
                    sib = sib | (ss << 6) | ((index & 0b111) << 3);
                } else {
                    sib = sib | (0b00100 << 3);
                }
                self.sib = Some(sib);
            }
        }
        self
    }

    pub(super) fn immediate(mut self, imm: Value, size: Size) -> Self {
        self.immediate = Some((imm, size));
        self
    }

    pub(super) fn build(self) -> Option<Instruction> {
        let len = self.opcode.len();
        let mut opcode = self.opcode;
        if len == 0 {
            return None;
        }
        // prefix
        let mut prefix: Vec<u8> = Vec::new();
        if let Some(seg) = self.segment_override {
            let code = match seg {
                Segment::Cs => 0x2e,
                Segment::Ds => 0x3e,
                Segment::Es => 0x26,
                Segment::Fs => 0x64,
                Segment::Gs => 0x65,
                Segment::Ss => 0x36,
            };
            prefix.push(code);
        }
        if self.operand_size_override {
            prefix.push(0x66);
        }
        // opcode+rd
        if let Some(rd) = self.opcode_rd {
            opcode[len - 1] += rd;
        }
        Some(
            Instruction {
                prefix,
                opcode,
                modrm: self.modrm,
                sib: self.sib,
                displacement: self.displacement,
                immediate: self.immediate
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{generator::{Statement, Value, Segment}, common::Size};

    use super::Instruction;


    #[test]
    fn test_builder_opcode() {
        // ret
        let ret = Instruction::builder().opcode(&[0xc3]).build().unwrap();
        let (text, _) = ret.emit();
        assert_eq!(&text, &[0xc3]);
    }

    #[test]
    fn test_builder_imm_operand() {
        // ret $8
        let ret = Instruction::builder()
            .opcode(&[0xc2])
            .immediate(Value::Integer(8), Size::Word)
            .build()
            .unwrap();
        let (text, _) = ret.emit();
        assert_eq!(&text, &[0xc2, 0x08, 0x00]);
    }

    #[test]
    fn test_builder_opcode_rd() {
        // pushl %ebx
        let inst = Instruction::builder()
            .opcode(&[0x50])
            .opcode_rd(3)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x53]);
    }

    #[test]
    fn test_builder_operand_size_prefix() {
        // pushw %ax
        let inst = Instruction::builder()
            .opcode(&[0x50])
            .opcode_rd(0)
            .operand_size_override_if(true)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x66, 0x50]);
    }

    #[test]
    fn test_builder_modrm_reg_opcode_reg() {
        // movl %ecx, 3
        let inst = Instruction::builder()
            .opcode(&[0x89])
            .modrm_reg_opcode(1) // ecx
            .modrm_rm_m(Some(Value::Integer(3)), None, None)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x89, 0x0d, 0x03, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_builder_modrm_rm_r() {
        // movl %ecx, %ebx
        let inst = Instruction::builder()
            .opcode(&[0x89])
            .modrm_reg_opcode(1) // ecx
            .modrm_rm_r(3) // ebx
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x89, 0xcb]);
        // movl %ecx, %ebx
        let inst = Instruction::builder()
            .opcode(&[0x8b])
            .modrm_reg_opcode(3) // ebx
            .modrm_rm_r(1) // ecx
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8b, 0xd9]);
    }

    #[test]
    fn test_builder_mem_disp() {
        // popl 3
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(Some(Value::Integer(3)), None, None)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x05, 0x03, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_builder_mem_base() {
        // popl (%eax)
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(None, Some(0), None)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x00]);
    }

    #[test]
    fn test_builder_mem_base_disp8() {
        // popl 3(%eax)
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(Some(Value::Integer(3)), Some(0), None)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x40, 0x03]);
    }

    #[test]
    fn test_builder_mem_base_disp32() {
        // popl 0x2030(%eax)
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_rm_m(Some(Value::Integer(0x2030)), Some(0), None)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x80, 0x30, 0x20, 0x00, 0x00]);
    }

    #[test]
    fn test_builder_mem_index() {
        // popl (,%eax, 1)
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(None, None, Some((0, 1)))
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x04, 0x05, 0x00, 0x00, 0x00, 0x00])
    }

    #[test]
    fn test_builder_mem_base_index_disp() {
        // popl 3(%eax, %ebx, 2)
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(Some(Value::Integer(3)), Some(0), Some((3, 2)))
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x44, 0x58, 0x03]);
    }

    // 以下是特例
    #[test]
    fn test_builder_mem_base_esp() {
        // popl (%esp)
        // base 为 %esp 即使无 index, scale 也要使用 sib
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(None, Some(4), None)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x04, 0x24]);
    }

    #[test]
    fn test_builder_mme_base_ebp() {
        // popl (%ebp)
        // base 为 %ebp 且无 disp 时, 认为 disp 为 0u8
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(None, Some(5), None)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x45, 0x00]);
    }

    #[test]
    fn test_builder_mem_base_ebp_index() {
        // popl (%ebp, %eax)
        // base 为 %ebp 且无 disp 时, 认为 disp 为 0u8
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(None, Some(5), Some((0, 1)))
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x8f, 0x44, 0x05, 0x00]);
    }

    #[test]
    fn test_builder_segment_overide_prefix() {
        // popl %cs:(%eax)
        let inst = Instruction::builder()
            .opcode(&[0x8f])
            .modrm_reg_opcode(0)
            .modrm_rm_m(None, Some(0), None)
            .segment_override(Segment::Cs)
            .build()
            .unwrap();
        let (text, _) = inst.emit();
        assert_eq!(&text, &[0x2e, 0x8f, 0x00]);
    }
}