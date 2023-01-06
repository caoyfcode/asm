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
    IPopa, IPopad, IPopf, IPopfd,
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
    // comment
    Comment,
    // 特殊符号
    Eol, Err(String)
}

macro_rules! regex_token_vec {
    ( $($r:expr => $t:expr),* $(,)? ) => {
        vec![
            $( (Regex::new($r).unwrap(), $t) ),*
        ]
    };
}

lazy_static! {
    static ref SIMPLE_PATTERNS: Vec<(Regex, Token)> = regex_token_vec! [
        // pseudo
        r"^\.section" => Token::DotSection,
        r"^\.(global|globl)" => Token::DotGlobal,
        r"^\.(equ|set)" => Token::DotEqu,
        r"^\.fill" => Token::DotFill,
        r"^\.byte" => Token::DotByte,
        r"^\.(word|short)" => Token::DotWord,
        r"^\.(long|int)" => Token::DotLong,
        r"^\.ascii" => Token::DotAscii,
        r"^\.(asciz|string)" => Token::DotAsciz,
        r"^\.comm" => Token::DotComm,
        r"^\.lcomm" => Token::DotLcomm,
        // instructions
        // registers
        // 操作符
        r"^\(" => Token::Lparen, r"^\)" => Token::Rparen,
        r"^:" => Token::Colon, r"^," => Token::Comma,
        r"^\$" => Token::Dollar, r"^\*" => Token::Star,
        r"^(#|;|//).*$" => Token::Comment,
    ];

    static ref INTEGER: Regex = Regex::new(
        r"(?P<bin>^0b[01]+)|(?P<hex>^0x[0-9a-f]+)|(?P<dec>^[1-9][0-9]*)|(?P<oct>^0[0-7]*)|(?P<char>^'[ -~]')"
    ).unwrap();

    static ref STRING: Regex = Regex::new(
        r#"^"[ -~]*""#
    ).unwrap();

    static ref SYMBOL: Regex = Regex::new(r"^[a-zA-Z_.][a-zA-Z0-9_.]*").unwrap();
}

impl Token {

    /// 从 str 开头匹配一个非特殊符号的文法符号,
    /// 返回匹配到的符号与符号的下一个字符位置
    fn matching(str: &str) -> (Token, usize) {
        let len = str.len();
        let str = str.trim_start();
        let trim_len = len - str.len();
        // simple patterns
        for (reg, token) in &*SIMPLE_PATTERNS {
            if let Some(m) = reg.find(str) {
                return (token.clone(), trim_len + m.end());
            }
        }
        // integer
        if let Some(cap) = INTEGER.captures(str) {
            if let Some(bin) = cap.name("bin") {
                let end = bin.end();
                return match u32::from_str_radix(&str[2..end], 2) {
                    Ok(integer) => (Token::Integer(integer), trim_len + end),
                    Err(err) => (Token::Err(err.to_string()), trim_len),
                };
            }
            if let Some(oct) = cap.name("oct") {
                let end = oct.end();
                return match u32::from_str_radix(&str[0..end], 8) { // 0 被归到 8 进制
                    Ok(integer) => (Token::Integer(integer), trim_len + end),
                    Err(err) => (Token::Err(err.to_string()), trim_len),
                };
            }
            if let Some(hex) = cap.name("hex") {
                println!("hex: {}", hex.as_str());
                let end = hex.end();
                return match u32::from_str_radix(&str[2..end], 16) {
                    Ok(integer) => (Token::Integer(integer), trim_len + end),
                    Err(err) => (Token::Err(err.to_string()), trim_len),
                };
            }
            if let Some(dec) = cap.name("dec") {
                let end = dec.end();
                return match u32::from_str_radix(&str[0..end], 10) {
                    Ok(integer) => (Token::Integer(integer), trim_len + end),
                    Err(err) => (Token::Err(err.to_string()), trim_len),
                };
            }
            if let Some(_) = cap.name("char") {
                return (Token::Integer(str.bytes().nth(1).unwrap() as u32), trim_len + 3);
            }
        }
        // string
        if let Some(m) = STRING.find(str) {
            let end = m.end();
            return  (Token::String(String::from(&str[1..(end - 1)])), trim_len + end);
        }
        // symbol
        if let Some(m) = SYMBOL.find(str) {
            return (Token::Symbol(String::from(m.as_str())), trim_len + m.end());
        }
        // err
        (Token::Err(String::from("unknown symbol")), trim_len)
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
    type Item = Token; // 返回 Token 中除了 Comment 之外的任何符号

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
            } else if self.started { // 除了首行外每一行开头 Eol
                return Some(Token::Eol);
            } else {
                self.started = true;
            }
        }
        let str = &self.buffer[self.cursor..];
        let (token, size) = Token::matching(str);
        self.cursor += size;
        if token == Token::Comment {
            self.next()
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod pseudo_tests {
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
                Token::Eol,
                Token::DotLcomm,
                Token::Symbol(String::from("b")),
                Token::Comma,
                Token::Integer(8),
            ]
        );
    }

    #[test]
    fn test_comment() {
        use Token::*;
        let str = r".byte 20, 22 # a byte
                        .word 300 # a word
                        .short 300 ; a word
                        .long 70000 // a long (double word)
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
    fn test_all_pseudo() {
        let str = r#"
        .section .text
        .global main
        main: # main function
        .section .data ; data section
        hello: .string "Hello, World"
        num: .int 0xf0
        .equ haha, 0b1101
        .section .bss
        .lcomm buffer, 16"#.trim();
        let tokens = vec![
            Token::DotSection, Token::Symbol(String::from(".text")), Token::Eol,
            Token::DotGlobal, Token::Symbol(String::from("main")), Token::Eol,
            Token::Symbol(String::from("main")), Token::Colon, Token::Eol,
            Token::DotSection, Token::Symbol(String::from(".data")), Token::Eol,
            Token::Symbol(String::from("hello")), Token::Colon,
            Token::DotAsciz, Token::String(String::from("Hello, World")), Token::Eol,
            Token::Symbol(String::from("num")), Token::Colon,
            Token::DotLong, Token::Integer(0xf0), Token::Eol,
            Token::DotEqu, Token::Symbol(String::from("haha")), Token::Comma, Token::Integer(0b1101), Token::Eol,
            Token::DotSection, Token::Symbol(String::from(".bss")), Token::Eol,
            Token::DotLcomm, Token::Symbol(String::from("buffer")), Token::Comma, Token::Integer(16),
        ];
        test_str(str, tokens);
    }

}