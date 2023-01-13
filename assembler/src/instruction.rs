use lazy_static::lazy_static;


lazy_static! {
    // 所有不带大小后缀的指令
    static ref MNEMONICS_WITHOUT_SIZE: Vec<&'static str> = vec![
        "pusha", "pushad", "pushf", "pushfd",
        "popa", "popad", "popf", "popfd",
        "ret",
        "int", "dec", "inc",
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
        "eax", "ax", "ah", "al",
        "ebx", "bx", "bh", "bl",
        "ecx", "cx", "ch", "cl",
        "edx", "dx", "dh", "dl",
        "esi", "si", "edi", "di",
        "eflags", "flags", "eip", "ip",
        "cs", "ds", "es", "fs", "gs", "ss",
    ];

    // 跳转指令(操作数语法与一般指令不同)的助记符
    static ref JUMP_MNEMONICS: Vec<&'static str> = vec![
        "jmp", "call",
        "ja", "jae", "jb", "jbe", "je", "jna", "jnae", "jnb", "jnbe", "jne"
    ];
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
pub fn is_jump(mnemonic: &str) -> bool {
    JUMP_MNEMONICS.contains(&mnemonic)
}

#[cfg(test)]
mod tests {
    use super::is_jump;

    #[test]
    fn test_is_jump() {
        assert!(is_jump("jmp"));
        assert!(is_jump("call"));
        assert!(is_jump("jne"));
        assert!(!is_jump("mov"));
    }
}