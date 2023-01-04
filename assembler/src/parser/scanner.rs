use std::io::BufRead;

use crate::common::Size;

#[derive(Debug, PartialEq)]
pub(super) enum Token {
    // pseudo
    DotSection, DotGlobal, DotEqu,
    DotFill, DotByte, DotWord, DotLong, DotAscii, DotAsciz,
    DotComm, DotLcomm,
    // mnemonic 0op
    IPusha, IPushad, IPushf, IPushfd,
    IPopa, IPopad, IPopd, IPopfd,
    IRet,
    // mnemonic 1op
    IInt, IDec, IInc,
    IMul(Option<Size>), IDiv(Option<Size>),
    // mnemonic 2op
    IMov(Option<Size>), ICmp(Option<Size>),
    IAdd(Option<Size>), ISub(Option<Size>),
    IAnd(Option<Size>), IOr(Option<Size>),
    IXor(Option<Size>), ITest(Option<Size>), ILea(Option<Size>),
    // mnemonic jump
    IJmp, ICall,
    IJa, IJae, IJb, IJbe, IJc, IJe,
    IJna, IJnae, IJnb, IJnbe, IJnc, IJne,
    // registers
    RegEax, RegAx, RegAh, RegAl,
    RegEbx, RegBx, RegBh, RegBl,
    RegEcx, RegCx, RegCh, RegCl,
    RegEdx, RegDx, RegDh, RegDl,
    RegEbp, RegBp,
    RegEsi, RegSi,
    RegEdi, RegDi,
    RegEsp, RegSp,
    RegEflags, RegFlags,
    RegEip, RegIp,
    RegCs, RegDs, RegEs, RegSs, RegFs, RegGs,
    // 操作符
    Lparen, Rparen, Colon, Comma, Dollar, Star,
    // data
    Integer(u32),
    String(String),
    // identifier
    Symbol(String),
    // 特殊符号
    Eol, Err(String)
}

pub(super) struct Scanner<R: BufRead> {
    reader: R,
}

impl<R: BufRead> Scanner<R> {
    pub(super) fn new(reader: R) -> Self {
        Self {
            reader,
        }
    }
}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::{Scanner, Token};

    fn test_str(str: &str, tokens: Vec<Token>) {
        let cursor = Cursor::new(str);
        let mut scanner = Scanner::new(cursor);

        for token in tokens {
            assert_eq!(scanner.next(), Some(token));
        }
        assert_eq!(scanner.next(), None);
    }


    #[test]
    fn test_section_symbol() {
        test_str(
            ".section .text\n.section .data",
            vec![
                Token::DotSection,
                Token::Symbol(String::from(".text")),
                Token::Eol,
                Token::DotSection,
                Token::Symbol(String::from(".data")),
            ]
        );
    }

    #[test]
    fn test_global_symbol() {
        test_str(
            ".global _start\n.globl main",
            vec![
                Token::DotGlobal,
                Token::Symbol(String::from("_start")),
                Token::Eol,
                Token::DotGlobal,
                Token::Symbol(String::from("main")),
            ]
        );
    }

    #[test]
    fn test_equ_symbol_comma_value() {
        test_str(
            ".equ len, 3\n.set a, len",
            vec![
                Token::DotEqu,
                Token::Symbol(String::from("len")),
                Token::Comma,
                Token::Integer(3),
                Token::Eol,
                Token::DotEqu,
                Token::Symbol(String::from("a")),
                Token::Comma,
                Token::Symbol(String::from("len")),
            ]
        );
    }

    #[test]
    fn test_fill_repeat_size_value() {
        test_str(
            ".fill 3, 2, 0",
            vec![
                Token::DotFill,
                Token::Integer(3),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(0),
                Token::Comma,
            ]
        );
    }

    #[test]
    fn test_data_section_integers() {
        use Token::*;
        let str = r".byte 20, 22
.word 300
.short 300
.long 70000
.int 70000";
        let tokens = vec![
            DotByte, Integer(20), Comma, Integer(22), Eol,
            DotWord, Integer(300), Eol,
            DotWord, Integer(300), Eol,
            DotLong, Integer(70000), Eol,
            DotLong, Integer(70000),
        ];

        test_str(str, tokens);
    }

    #[test]
    fn test_data_section_strings() {
        let str = r#".ascii "hello"
.asciz "hello"
.string "hello""#;
        let tokens = vec![
            Token::DotAscii, Token::String(String::from("hello")), Token::Eol,
            Token::DotAsciz, Token::String(String::from("hello")), Token::Eol,
            Token::DotAsciz, Token::String(String::from("hello"))
        ];

        test_str(str, tokens);
    }

    #[test]
    fn test_bss_section_decalarations() {
        test_str(
            ".comm a, 4\n.lcomm b, 8",
            vec![
                Token::DotComm,
                Token::Symbol(String::from("a")),
                Token::Comma,
                Token::Integer(4),
                Token::DotLcomm,
                Token::Symbol(String::from("b")),
                Token::Comma,
                Token::Integer(8),
            ]
        );
    }

}