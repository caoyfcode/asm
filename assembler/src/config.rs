use std::collections::{HashMap, HashSet};

use lazy_static::lazy_static;

use crate::common::Size;

#[derive(Debug)]
pub enum OperandEncoding {
    // 无操作数
    Zero, //  xxx : <opcode>
    // 1 个操作数
    Opcode, // xxx reg : <opcode+rd>
    ImpliedSreg(&'static str), // pop/push Sreg : <opcode> : 不用加 0x66 前缀
    Rm, // xxx r/m : <opcode> <modrm> (<sib>) (<disp>) : bool 表示 m 时是否需要后缀
    Mem, // xxx m : <opcode> <modrm> (<sib>) (<disp>) : bool 表示 m 时是否需要后缀
    Reg, // xxx reg : <opcode> <modrm> : 通用寄存器
    Imm(bool), // xxx imm : <opcode> <imm> : bool 表示 16 位操作数时是否需要加 0x66 前缀, 因为 ret imm16 就不需要
    Rel, // xxx label : <opcode> <imm> : 用于跳转指令, 未来可能添加 relaxation 功能
    // 2 个操作数 (src, dest)
    RegRm, // xxx reg, r/m : <opcode> <modrm> (<sib>) (<disp>)
    RmReg, // xxx r/m, reg : <opcode> <modrm> (<sib>) (<disp>)
    MemReg, // lea(w/l) m, reg : <opcode> <modrm> (<sib>) (<disp>)
    MoffsA, // xxx moffs, %al/%ax/%eax : <opcode> <disp32>
    AMoffs, // xxx %al/%ax/%eax, moffs : <opcode> <disp32>
    ImmOpcode, // xxx imm, reg : <opcode+rd> <imm>
    ImmRm, // xxx imm, r/m : <opcode> <modrm> (<sib>) (<disp>) <imm>
    ImmA, // xxx imm, %al/%ax/%eax : <opcode> <imm>
    SregRm, // xxx Sreg, r/m(16|32) : <opcode> <modrm> (<sib>) (<disp>)
    RmSreg, // xxx r/m16, Sreg : <opcode> <modrm> (<sib>) (<disp>) : m16 时不用加 0x66 前缀
}

pub struct InstructionInfo {
    pub opcode: Vec<u8>,
    pub modrm_opcode: Option<u8>,
    pub operand_size: Size, // size 为 word 时要加前缀, 对于 Zero 的指令, 有可能隐含操作数, 因而也要加前缀
    pub operand_encoding: OperandEncoding,
    pub is_default: bool, // 当操作数大小无法确定时, 是否要作为默认使用
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
    ( $( $name:expr => [ $( ( $opcode:expr, $modrm_opcode:expr, $size:expr, $encoding:expr $(, $is:expr)? ) ),* $(,)? ] ),* $(,)? ) => {
        HashMap::from([
            $((
                $name,
                vec![ $( InstructionInfo {
                    opcode: $opcode,
                    modrm_opcode: $modrm_opcode,
                    operand_size: $size,
                    operand_encoding: $encoding,
                    is_default: false $( || $is)?,
                } ),* ]
            )),*
        ])
    };
}

macro_rules! hash_map {
    ( $( $name:expr => $info:expr ),* $(,)? ) => {
        HashMap::from([
            $(
                ($name, $info)
            ),*
        ])
    }
}

macro_rules! hash_set {
    ( $( $ele:expr ),* $(,)? ) => {
        HashSet::from([
            $(
                $ele
            ),*
        ])
    }
}

lazy_static! {
    // 所有寄存器信息
    static ref REGISTER_INFOS: HashMap<&'static str, RegisterInfo> = hash_map! {
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

    // 所有指令信息
    static ref INSTRUCTION_INFOS: HashMap<&'static str, Vec<InstructionInfo>> = instruction_map! {
        // "mnemonic" => [ ( opcode, modrm_opcode, size, operand_encoding (, is_defalut)? ),* ]
        // -- Load and Move Instructions --
        "lea" => [
            (vec![0x8d], None, Size::Word, OperandEncoding::MemReg), // leaw m16, r16
            (vec![0x8d], None, Size::DoubleWord, OperandEncoding::MemReg), // leal m32, r32
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
            (vec![0x8e], None, Size::Word, OperandEncoding::RmSreg, true), // movw r/m16, sreg
            (vec![0xb0], None, Size::Byte, OperandEncoding::ImmOpcode), // movb imm8, r8
            (vec![0xb8], None, Size::Word, OperandEncoding::ImmOpcode), // movw imm16, r16
            (vec![0xb8], None, Size::DoubleWord, OperandEncoding::ImmOpcode), // movl imm32, r32
            (vec![0xc6], Some(0), Size::Byte, OperandEncoding::ImmRm), // movb imm8, r/m8
            (vec![0xc7], Some(0), Size::Word, OperandEncoding::ImmRm), // movw imm16, r/m16
            (vec![0xc7], Some(0), Size::DoubleWord, OperandEncoding::ImmRm), // movl imm32, r/m32
        ],
        // -- Arithmetic Logical Instructions --
        "add" => [
            (vec![0x04], None, Size::Byte, OperandEncoding::ImmA), // addb imm8, %al
            (vec![0x05], None, Size::Word, OperandEncoding::ImmA), // addw imm16, %ax
            (vec![0x05], None, Size::DoubleWord, OperandEncoding::ImmA), // addl imm32, %eax
            (vec![0x80], Some(0), Size::Byte, OperandEncoding::ImmRm), // addb imm8, r/m8
            (vec![0x81], Some(0), Size::Word, OperandEncoding::ImmRm), // addw imm16, r/m16
            (vec![0x81], Some(0), Size::DoubleWord, OperandEncoding::ImmRm), // addl imm32, r/m32
            (vec![0x00], None, Size::Byte, OperandEncoding::RegRm), // addb r8, r/m8
            (vec![0x01], None, Size::Word, OperandEncoding::RegRm), // addw r16, r/m16
            (vec![0x01], None, Size::DoubleWord, OperandEncoding::RegRm), // addl r32, r/m32
            (vec![0x02], None, Size::Byte, OperandEncoding::RmReg), // addb r/m8, r8
            (vec![0x03], None, Size::Word, OperandEncoding::RmReg), // addw r/m16, r16
            (vec![0x03], None, Size::DoubleWord, OperandEncoding::RmReg), // addl r/m32, r32
        ],
        // -- Multiply and Divide Instructions --
        // -- Procedure Call and Return Instructions --
        "ret" => [
            (vec![0xc3], None, Size::Byte, OperandEncoding::Zero, true), // ret
            (vec![0xc2], None, Size::Word, OperandEncoding::Imm(false), true), // ret imm16
        ],
        "call" => [
            (vec![0xe8], None, Size::Word, OperandEncoding::Rel), // call rel16
            (vec![0xe8], None, Size::DoubleWord, OperandEncoding::Rel, true), // call rel32
            (vec![0xff], Some(2), Size::Word, OperandEncoding::Rm), // call *r/m16
            (vec![0xff], Some(2), Size::DoubleWord, OperandEncoding::Rm, true), // call *r/m32
        ],
        // -- Jump Instructions --
        "jmp" => [
            (vec![0xe8], None, Size::Byte, OperandEncoding::Rel), // jmp rel8
            (vec![0xe9], None, Size::Word, OperandEncoding::Rel), // jmp rel16
            (vec![0xe9], None, Size::DoubleWord, OperandEncoding::Rel, true), // jmp rel32
            (vec![0xff], Some(4), Size::Word, OperandEncoding::Rm), // jmp *r/m16
            (vec![0xff], Some(4), Size::DoubleWord, OperandEncoding::Rm, true), // jmp *r/m32
        ],
        // jcc
        "ja" => [
            (vec![0x77], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x87], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x87], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jae" => [
            (vec![0x73], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x83], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x83], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jb" => [
            (vec![0x72], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x82], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x82], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jbe" => [
            (vec![0x76], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x86], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x86], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "je" => [
            (vec![0x74], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x84], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x84], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jg" => [
            (vec![0x7f], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x8f], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x8f], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jge" => [
            (vec![0x7d], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x8d], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x8d], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jl" => [
            (vec![0x7c], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x8c], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x8c], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jle" => [
            (vec![0x7e], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x8e], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x8e], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jne" => [
            (vec![0x75], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x85], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x85], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jno" => [
            (vec![0x71], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x81], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x81], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jnp" => [
            (vec![0x7b], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x8b], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x8b], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jns" => [
            (vec![0x79], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x89], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x89], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jo" => [
            (vec![0x70], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x80], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x80], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "jp" => [
            (vec![0x7a], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x8a], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x8a], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        "js" => [
            (vec![0x78], None, Size::Byte, OperandEncoding::Rel),
            (vec![0x0f, 0x88], None, Size::Word, OperandEncoding::Rel),
            (vec![0x0f, 0x88], None, Size::DoubleWord, OperandEncoding::Rel, true),
        ],
        // -- Conversion Instructions --
        // -- Push and Pop Instructions --
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
            (vec![0x6a], None, Size::Byte, OperandEncoding::Imm(true)), // pushb imm8
            (vec![0x68], None, Size::Word, OperandEncoding::Imm(true)), // pushw imm16
            (vec![0x68], None, Size::DoubleWord, OperandEncoding::Imm(true)), // pushl imm32
            (vec![0x0e], None, Size::Word, OperandEncoding::ImpliedSreg("cs")), // push %cs
            (vec![0x16], None, Size::Word, OperandEncoding::ImpliedSreg("ss")), // push %ss
            (vec![0x1e], None, Size::Word, OperandEncoding::ImpliedSreg("ds")), // push %ds
            (vec![0x06], None, Size::Word, OperandEncoding::ImpliedSreg("es")), // push %es
            (vec![0x0f, 0xa0], None, Size::Word, OperandEncoding::ImpliedSreg("fs")), // push %fs
            (vec![0x0f, 0xa8], None, Size::Word, OperandEncoding::ImpliedSreg("gs")), // push %gs
        ],
        "pusha" => [ // push 所有通用寄存器
            (vec![0x60], None, Size::Word, OperandEncoding::Zero), // pushaw
            (vec![0x60], None, Size::DoubleWord, OperandEncoding::Zero, true), // pusha/pushal
        ],
        "pushf" => [ // push eflags/flags
            (vec![0x9c], None, Size::Word, OperandEncoding::Zero), // pushfw
            (vec![0x9c], None, Size::DoubleWord, OperandEncoding::Zero, true), // pushf/pushfl
        ],
        "popa" => [ // pop 所有通用寄存器
            (vec![0x61], None, Size::Word, OperandEncoding::Zero), // popaw
            (vec![0x61], None, Size::DoubleWord, OperandEncoding::Zero, true), // popa/popal
        ],
        "popf" => [ // pop eflags/flags
            (vec![0x9d], None, Size::Word, OperandEncoding::Zero), // popfw
            (vec![0x9d], None, Size::DoubleWord, OperandEncoding::Zero, true), // popf/popfl
        ],
        // -- Rotate Instructions --
        // -- Bit Instructions --
        // -- Byte Instructions --
        // -- Flag Instructions --
        // -- Interrupt Instructions --
        "int" => [
            (vec![0xcd], None, Size::Byte, OperandEncoding::Imm(false), true), // int imm8
        ],
        "int3" => [
            (vec![0xcc], None, Size::Byte, OperandEncoding::Zero, true), // int3
        ],
        "int1" => [
            (vec![0xf1], None, Size::Byte, OperandEncoding::Zero, true), // int1
        ],
        "into" => [
            (vec![0xce], None, Size::Byte, OperandEncoding::Zero, true), // into
        ],
        // -- Other Instructions --
        "nop" => [
            (vec![0x90], None, Size::Byte, OperandEncoding::Zero, true), // nop
        ],
    };

    static ref INSTRUCTION_ALIAS: HashMap<&'static str, &'static str> = hash_map! {
        "jc" => "jb",
        "jna" => "jbe",
        "jnae" => "jb",
        "jnb" => "jae",
        "jnbe" => "ja",
        "jnc" => "jae",
        "jng" => "jle",
        "jnge" => "jl",
        "jnl" => "jge",
        "jnle" => "jg",
        "jnz" => "jne",
        "jpe" => "jp",
        "jpo" => "jnp",
        "jz" => "je",
    };

    // 跳转指令(操作数语法与一般指令不同)的助记符
    static ref JUMP_MNEMONICS: HashSet<&'static str> = hash_set![
        "jmp", "call",
        "ja", "jae", "jb", "jbe", "je", "jg", "jge",
        "jl", "jle", "jne", "jno", "jnp", "jns", "jo", "jp", "js",
        // alias
        "jc", "jna", "jnae", "jnb", "jnbe", "jnc", "jng", "jnge",
        "jnl", "jnle", "jnz", "jpe", "jpo", "jz",
    ];

    // 不可以带大小后缀的指令助记符
    static ref NO_SIZE_MNEMONICS: HashSet<&'static str> = hash_set![
        "ret", "nop",
        "int", "int3", "int1", "into", "dec", "inc",
        // jcc
        "ja", "jae", "jb", "jbe", "je", "jg", "jge",
        "jl", "jle", "jne", "jno", "jnp", "jns", "jo", "jp", "js",
        "jc", "jna", "jnae", "jnb", "jnbe", "jnc", "jng", "jnge",
        "jnl", "jnle", "jnz", "jpe", "jpo", "jz",

    ];
}

/// 判断是否是合法的指令助记符, 合法则返回 (裸助记符, 大小后缀, 是否是跳转指令)
pub fn info_of_mnemonic(mnemonic: &str) -> Option<(&str, Option<Size>, bool)> {
    let (mnemonic, size) = if !infos_of_instruction(mnemonic).is_some() {
        let len = mnemonic.len();
        let size = match mnemonic.as_bytes()[len - 1] as char {
            'b' => Size::Byte,
            'w' => Size::Word,
            'l' => Size::DoubleWord,
            _ => return None,
        };
        let bare_mnemonic = &mnemonic[..len - 1];
        if infos_of_instruction(bare_mnemonic).is_some() && !NO_SIZE_MNEMONICS.contains(bare_mnemonic) {
            (bare_mnemonic, Some(size))
        } else {
            return None;
        }
    } else {
        (mnemonic, None)
    };
    Some((
        mnemonic,
        size,
        JUMP_MNEMONICS.contains(mnemonic)
    ))
}

pub fn info_of_register(name: &str) -> Option<&'static RegisterInfo> {
    REGISTER_INFOS.get(name)
}

pub fn infos_of_instruction(mnemonic: &str) -> Option<&'static Vec<InstructionInfo>> {
    let mnemonic = match INSTRUCTION_ALIAS.get(mnemonic) {
        Some(mne) => *mne,
        None => mnemonic,
    };
    INSTRUCTION_INFOS.get(mnemonic)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mnemonic_with_size() {
        assert_eq!(
            info_of_mnemonic("mov"),
            Some(("mov", None, false))
        );
        assert_eq!(
            info_of_mnemonic("movl"),
            Some(("mov", Some(Size::DoubleWord), false))
        );
    }

    #[test]
    fn test_mnemonic_no_size() {
        assert_eq!(
            info_of_mnemonic("int"),
            Some(("int", None, false))
        );
        assert_eq!(
            info_of_mnemonic("intl"),
            None
        );
    }

    #[test]
    fn test_mnemonic_jump() {
        assert_eq!(
            info_of_mnemonic("jmp"),
            Some(("jmp", None, true))
        );
        assert_eq!(
            info_of_mnemonic("je"),
            Some(("je", None, true))
        );
    }

    #[test]
    fn test_mnemonic_invalid() {
        assert_eq!(
            info_of_mnemonic("hehehe"),
            None
        );
    }
}