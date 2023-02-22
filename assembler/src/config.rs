use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::common::Size;

#[derive(Debug)]
pub enum OperandEncoding {
    // 无操作数
    Zero, //  xxx : <opcode>
    // 1 个操作数
    Opcode, // xxx reg : <opcode+rd>
    ImpliedSreg(&'static str), // pop/push Sreg : <opcode> : 不用加 0x66 前缀
    Rm, // xxx r/m : <opcode> <modrm> (<sib>) (<disp>)
    Mem, // xxx m : <opcode> <modrm> (<sib>) (<disp>)
    Reg, // xxx reg : <opcode> <modrm> : 通用寄存器
    Imm, // xxx imm : <opcode> <imm>
    Rel, // xxx rel : <opcode> <imm>
    // 2 个操作数 (src, dest)
    RegRm, // xxx reg, r/m : <opcode> <modrm> (<sib>) (<disp>)
    RmReg, // xxx r/m, reg : <opcode> <modrm> (<sib>) (<disp>)
    MoffsA, // xxx moffs, %al/%ax/%eax : <opcode> <disp32>
    AMoffs, // xxx %al/%ax/%eax, moffs : <opcode> <disp32>
    ImmOpcode, // xxx imm, reg : <opcode+rd> <imm>
    ImmRm, // xxx imm, r/m : <opcode> <modrm> (<sib>) (<disp>) <imm>
    MemReg, // lea(w/l) m, reg : <opcode> <modrm> (<sib>) (<disp>)
    ImmA, // xxx imm, %al/%ax/%eax : <opcode> <imm>
    SregRm, // xxx Sreg, r/m(16|32) : <opcode> <modrm> (<sib>) (<disp>)
    RmSreg, // xxx r/m16, Sreg : <opcode> <modrm> (<sib>) (<disp>) : m16 时不用加 0x66 前缀
}

pub struct InstructionInfo {
    pub opcode: Vec<u8>,
    pub modrm_opcode: Option<u8>,
    pub operand_size: Size, // size 为 word 时要加前缀, 对于无操作数的指令, 此位无意义
    pub operand_encoding: OperandEncoding,
}

#[derive(PartialEq, Eq)]
pub enum RegisterKind {
    GeneralPurpose,
    Segment,
    Eflags,
    Eip,
}

pub struct RegisterInfo {
    pub code: u8, // 3 bit, 用于在 modrm, sib 与 opcode+rd 中表示通用寄存器, 或者部分指令中 modrm 表示段寄存器
    pub size: Size,
    pub kind: RegisterKind,
}

macro_rules! instruction_map {
    ( $( $name:expr => [ $( ( $opcode:expr, $modrm_opcode:expr, $size:expr, $encoding:expr ) ),* $(,)? ] ),* $(,)? ) => {
        HashMap::from([
            $((
                $name,
                vec![ $( InstructionInfo {
                    opcode: $opcode,
                    modrm_opcode: $modrm_opcode,
                    operand_size: $size,
                    operand_encoding: $encoding,
                } ),* ]
            )),*
        ])
    };
}

macro_rules! register_map {
    ( $( $name:expr => $info:expr ),* $(,)? ) => {
        HashMap::from([
            $(
                ($name, $info)
            ),*
        ])
    }
}

lazy_static! {
    // 所有不带大小后缀的指令
    static ref MNEMONICS_WITHOUT_SIZE: Vec<&'static str> = vec![
        "pusha", "pushad", "pushf", "pushfd",
        "popa", "popad", "popf", "popfd",
        "ret",
        "int", "int3", "int1", "into", "dec", "inc",
        "jmp", "call",
        "ja", "jae", "jb", "jbe", "je", "jna", "jnae", "jnb", "jnbe", "jne"
    ];

    // 所有可能带大小后缀的指令
    static ref MNEMONICS_WITH_SIZE: Vec<&'static str> = vec![
        "pop", "push", "mul", "div",
        "mov", "cmp", "add", "sub", "or", "xor", "test", "lea",
        "imul", "idiv",
    ];

    // 所有寄存器名
    static ref REGISTERS: Vec<&'static str> = vec![
        // 通用寄存器
        "eax", "ax", "ah", "al",
        "ebx", "bx", "bh", "bl",
        "ecx", "cx", "ch", "cl",
        "edx", "dx", "dh", "dl",
        "esi", "si", "edi", "di",
        "ebp", "bp", "esp", "sp",
        // 段寄存器
        "cs", "ds", "es", "fs", "gs", "ss",
        // eflags
        "eflags", "flags",
        // eip
        "eip", "ip",
    ];

    // 跳转指令(操作数语法与一般指令不同)的助记符
    static ref JUMP_MNEMONICS: Vec<&'static str> = vec![
        "jmp", "call",
        "ja", "jae", "jb", "jbe", "je", "jna", "jnae", "jnb", "jnbe", "jne"
    ];

    static ref REGISTER_INFOS: HashMap<&'static str, RegisterInfo> = register_map! {
        // 通用寄存器, code 可以用于 modrm, sib 与 opcode+rd
        "eax" => RegisterInfo { code: 0, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "ecx" => RegisterInfo { code: 1, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "edx" => RegisterInfo { code: 2, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "ebx" => RegisterInfo { code: 3, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "esp" => RegisterInfo { code: 4, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "ebp" => RegisterInfo { code: 5, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "esi" => RegisterInfo { code: 6, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "edi" => RegisterInfo { code: 7, size: Size::DoubleWord, kind: RegisterKind::GeneralPurpose },
        "ax" => RegisterInfo { code: 0, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "cx" => RegisterInfo { code: 1, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "dx" => RegisterInfo { code: 2, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "bx" => RegisterInfo { code: 3, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "sp" => RegisterInfo { code: 4, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "bp" => RegisterInfo { code: 5, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "si" => RegisterInfo { code: 6, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "di" => RegisterInfo { code: 7, size: Size::Word, kind: RegisterKind::GeneralPurpose },
        "al" => RegisterInfo { code: 0, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        "cl" => RegisterInfo { code: 1, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        "dl" => RegisterInfo { code: 2, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        "bl" => RegisterInfo { code: 3, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        "ah" => RegisterInfo { code: 4, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        "ch" => RegisterInfo { code: 5, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        "dh" => RegisterInfo { code: 6, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        "bh" => RegisterInfo { code: 7, size: Size::Byte, kind: RegisterKind::GeneralPurpose },
        // 段寄存器, code 可以用于 modrm
        "es" => RegisterInfo { code: 0, size: Size::Word, kind: RegisterKind::Segment },
        "cs" => RegisterInfo { code: 1, size: Size::Word, kind: RegisterKind::Segment },
        "ss" => RegisterInfo { code: 2, size: Size::Word, kind: RegisterKind::Segment },
        "ds" => RegisterInfo { code: 3, size: Size::Word, kind: RegisterKind::Segment },
        "fs" => RegisterInfo { code: 4, size: Size::Word, kind: RegisterKind::Segment },
        "gs" => RegisterInfo { code: 5, size: Size::Word, kind: RegisterKind::Segment },
        // 其他寄存器, code 无意义
        "eflags" => RegisterInfo { code: 0, size: Size::DoubleWord, kind: RegisterKind::Eflags },
        "flags" => RegisterInfo { code: 0, size: Size::Word, kind: RegisterKind::Eflags },
        "eip" => RegisterInfo { code: 0, size: Size::DoubleWord, kind: RegisterKind::Eip },
        "ip" => RegisterInfo { code: 0, size: Size::Word, kind: RegisterKind::Eip },
    };

    static ref INSTRUCTION_INFOS: HashMap<&'static str, Vec<InstructionInfo>> = instruction_map! {
        // "mnemonic" => [ ( opcode, modrm_opcode, size, operand_encoding ),* ]
        "ret" => [
            (vec![0xc3], None, Size::Byte, OperandEncoding::Zero), // ret
            (vec![0xc2], None, Size::Word, OperandEncoding::Imm), // ret imm16
        ],
        "int" => [
            (vec![0xcd], None, Size::Byte, OperandEncoding::Imm), // int imm8
        ],
        "int3" => [
            (vec![0xcc], None, Size::Byte, OperandEncoding::Zero), // int3
        ],
        "int1" => [
            (vec![0xf1], None, Size::Byte, OperandEncoding::Zero), // int1
        ],
        "into" => [
            (vec![0xce], None, Size::Byte, OperandEncoding::Zero), // into
        ],
        "pop" => [
            (vec![0x58], None, Size::Word, OperandEncoding::Opcode), // popw r16
            (vec![0x58], None, Size::DoubleWord, OperandEncoding::Opcode), // popl r32
            (vec![0x8f], Some(0), Size::Word, OperandEncoding::Mem), // popw m16
            (vec![0x8f], Some(0), Size::DoubleWord, OperandEncoding::Mem), // popl m32
            (vec![0x1f], None, Size::Word, OperandEncoding::ImpliedSreg("ds")), // pop %ds
            (vec![0x07], None, Size::Word, OperandEncoding::ImpliedSreg("es")), // pop %es
            (vec![0x17], None, Size::Word, OperandEncoding::ImpliedSreg("ss")), // pop %ss
            (vec![0x0f, 0xa1], None, Size::Word, OperandEncoding::ImpliedSreg("fs")), // pop %fs
            (vec![0x0f, 0xa9], None, Size::Word, OperandEncoding::ImpliedSreg("gs")), // pop %gs
        ],
        "push" => [
            (vec![0x50], None, Size::Word, OperandEncoding::Opcode), // pushw r16
            (vec![0x50], None, Size::DoubleWord, OperandEncoding::Opcode), // pushl r32
            (vec![0xff], Some(6), Size::Word, OperandEncoding::Rm), // pushw r/m16
            (vec![0xff], Some(6), Size::DoubleWord, OperandEncoding::Rm), // pushl r/m32
            (vec![0x6a], None, Size::Byte, OperandEncoding::Imm), // pushb imm8
            (vec![0x68], None, Size::Word, OperandEncoding::Imm), // pushw imm16
            (vec![0x68], None, Size::DoubleWord, OperandEncoding::Imm), // pushl imm32
            (vec![0x0e], None, Size::Word, OperandEncoding::ImpliedSreg("cs")), // push %cs
            (vec![0x16], None, Size::Word, OperandEncoding::ImpliedSreg("ss")), // push %ss
            (vec![0x1e], None, Size::Word, OperandEncoding::ImpliedSreg("ds")), // push %ds
            (vec![0x06], None, Size::Word, OperandEncoding::ImpliedSreg("es")), // push %es
            (vec![0x0f, 0xa0], None, Size::Word, OperandEncoding::ImpliedSreg("fs")), // push %fs
            (vec![0x0f, 0xa8], None, Size::Word, OperandEncoding::ImpliedSreg("gs")), // push %gs
        ],
        "mov" => [
            (vec![0xa0], None, Size::Byte, OperandEncoding::MoffsA), // movb moffs8, %al
            (vec![0xa1], None, Size::Word, OperandEncoding::MoffsA), // movw moffs16, %ax
            (vec![0xa1], None, Size::DoubleWord, OperandEncoding::MoffsA), // movl moffs32, %eax
            (vec![0xa2], None, Size::Byte, OperandEncoding::AMoffs), // movb %al, moffs8
            (vec![0xa3], None, Size::Word, OperandEncoding::AMoffs), // movw %ax, moffs16
            (vec![0xa3], None, Size::DoubleWord, OperandEncoding::AMoffs), // movl %eax, moffs32
            (vec![0x88], None, Size::Byte, OperandEncoding::RegRm), // movb r8, r/m8
            (vec![0x89], None, Size::Word, OperandEncoding::RegRm), // movw r16, r/m16
            (vec![0x89], None, Size::DoubleWord, OperandEncoding::RegRm), // movl r32, r/m32
            (vec![0x8a], None, Size::Byte, OperandEncoding::RmReg), // movb r/m8, r8
            (vec![0x8b], None, Size::Word, OperandEncoding::RmReg), // movb r/m16, r16
            (vec![0x8b], None, Size::DoubleWord, OperandEncoding::RmReg), // movb r/m32, r32
            (vec![0x8c], None, Size::Word, OperandEncoding::SregRm), // movw sreg, r/m16
            (vec![0x8c], None, Size::DoubleWord, OperandEncoding::SregRm), // movl sreg, r/m32
            (vec![0x8e], None, Size::Word, OperandEncoding::RmSreg), // movw r/m16, sreg
            (vec![0xb0], None, Size::Byte, OperandEncoding::ImmOpcode), // movb imm8, r8
            (vec![0xb8], None, Size::Word, OperandEncoding::ImmOpcode), // movw imm16, r16
            (vec![0xb8], None, Size::DoubleWord, OperandEncoding::ImmOpcode), // movl imm32, r32
            (vec![0xc6], Some(0), Size::Byte, OperandEncoding::ImmRm), // movb imm8, r/m8
            (vec![0xc7], Some(0), Size::Word, OperandEncoding::ImmRm), // movw imm16, r/m16
            (vec![0xc7], Some(0), Size::DoubleWord, OperandEncoding::ImmRm), // movl imm32, r/m32
        ],
        "lea" => [
            (vec![0x8d], None, Size::Word, OperandEncoding::MemReg), // leaw m16, r16
            (vec![0x8d], None, Size::DoubleWord, OperandEncoding::MemReg), // leal m32, r32
        ],
    };
}

/// 返回所有不带 `b|w|l` 后缀的指令助记符
pub fn mnemonics_without_size() -> &'static [&'static str] {
    &MNEMONICS_WITHOUT_SIZE
}

/// 返回所有可能带 `b|w|l` 后缀的指令助记符
pub fn mnemonics_with_size() -> &'static [&'static str] {
    &MNEMONICS_WITH_SIZE
}

/// 返回所有寄存器名
pub fn registers() -> &'static [&'static str] {
    &REGISTERS
}

/// 判断助记符是否是跳转指令, 在语法分析时用来确定操作数的语法
pub fn mnemonic_is_jump(mnemonic: &str) -> bool {
    JUMP_MNEMONICS.contains(&mnemonic)
}

pub fn info_of_register(name: &str) -> Option<&'static RegisterInfo> {
    REGISTER_INFOS.get(name)
}

pub fn infos_of_instruction(mnemonic: &str) -> Option<&'static Vec<InstructionInfo>> {
    INSTRUCTION_INFOS.get(mnemonic)
}

#[cfg(test)]
mod tests {
    use super::mnemonic_is_jump;

    #[test]
    fn test_is_jump() {
        assert!(mnemonic_is_jump("jmp"));
        assert!(mnemonic_is_jump("call"));
        assert!(mnemonic_is_jump("jne"));
        assert!(!mnemonic_is_jump("mov"));
    }
}