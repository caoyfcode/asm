use lazy_static::lazy_static;


lazy_static! {
    static ref JUMP_INSTRUCTIONS: Vec<&'static str> = vec![
        "jmp", "call",
        "ja", "jae", "jb", "jbe", "je", "jna", "jnae", "jnb", "jnbe", "jne"
    ];
}

pub fn is_jump(mnemonic: &str) -> bool {
    JUMP_INSTRUCTIONS.contains(&mnemonic)
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