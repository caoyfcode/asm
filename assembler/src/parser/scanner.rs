use std::io::BufRead;

use lazy_static::lazy_static;
use regex::Regex;

use crate::common::Size;

#[derive(Debug, PartialEq, Clone)]
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

lazy_static! {
    static ref SIMPLE_PATTERNS: Vec<(Regex, Token)> = vec![
        (Regex::new(r"^.section").unwrap(), Token::DotSection),
        (Regex::new(r"^(.global|.globl)").unwrap(), Token::DotGlobal),
    ];

    static ref SYMBOL: Regex = Regex::new(r"^[a-zA-Z_.][a-zA-Z0-9_.]*").unwrap();
}

impl Token {

    /// 从 str 开头匹配一个文法符号,
    /// 返回匹配到的符号与符号的下一个字符位置
    fn matching(str: &str) -> (Token, usize) {
        let len = str.len();
        let str = str.trim_start();
        let trim_len = len - str.len();

        for (reg, token) in &*SIMPLE_PATTERNS {
            if let Some(m) = reg.find(str) {
                return (token.clone(), trim_len + m.end())
            }
        }

        if let Some(m) = SYMBOL.find(str) {
            return (Token::Symbol(String::from(m.as_str())), trim_len + m.end());
        }

        return (Token::Err(String::from(str)), trim_len);
    }
}

pub(super) struct Scanner<R: BufRead> {
    reader: R,
    buffer: String,  // 缓存一行
    cursor: usize,  // 指示分析到 buffer 的位置
    started: bool,  // 是否已经开始分析
}

impl<R: BufRead> Scanner<R> {

    pub(super) fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: String::new(),
            cursor: 0,
            started: false,
        }
    }

}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cursor >= self.buffer.len() {
            self.buffer.clear();
            self.cursor = 0;
            let size = self.reader.read_line(&mut self.buffer).unwrap();
            if self.buffer.ends_with("\n") {
                self.buffer.pop();
            }
            if size == 0 {
                return None;
            } else if self.started {
                return Some(Token::Eol);
            } else {
                self.started = true;
            }
        }
        let str = &self.buffer[self.cursor..];
        let (token, size) = Token::matching(str);
        self.cursor += size;
        Some(token)
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