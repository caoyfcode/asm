use std::io::BufRead;

use lazy_static::lazy_static;
use regex::Regex;

use crate::common::{Size, Error};
use crate::instruction::{mnemonics_without_size, mnemonics_with_size, registers};

#[derive(Debug, PartialEq, Clone)]
pub(super) enum Token {
    // 伪指令
    DotSection, DotGlobal, DotEqu,
    DotFill, DotByte, DotWord, DotLong, DotAscii, DotAsciz,
    DotComm, DotLcomm,
    // 指令与寄存器
    Mnemonic(String, Option<Size>),  // String 储存不带后缀的助记符(小写)
    Register(String), // String 储存不带 % 的寄存器名(小写)
    // 操作符
    Lparen, Rparen, Colon, Comma, Dollar, Star,
    // 数据
    Integer(u32),
    String(String),
    // 标识符
    Symbol(String),
    // 注释
    Comment,
    // 特殊符号
    Eol,
    Err(Error)
}

macro_rules! regex_token_vec {
    ( $($r:expr => $t:expr),* $(,)? ) => {
        vec![
            $( (Regex::new($r).unwrap(), $t) ),*
        ]
    };
}

lazy_static! {
    static ref PSEUDOS: Vec<(Regex, Token)> = regex_token_vec![
        r"^\.section\b" => Token::DotSection,
        r"^\.(global|globl)\b" => Token::DotGlobal,
        r"^\.(equ|set)\b" => Token::DotEqu,
        r"^\.fill\b" => Token::DotFill,
        r"^\.byte\b" => Token::DotByte,
        r"^\.(word|short)\b" => Token::DotWord,
        r"^\.(long|int)\b" => Token::DotLong,
        r"^\.ascii\b" => Token::DotAscii,
        r"^\.(asciz|string)\b" => Token::DotAsciz,
        r"^\.comm\b" => Token::DotComm,
        r"^\.lcomm\b" => Token::DotLcomm,
        r"^\(" => Token::Lparen, r"^\)" => Token::Rparen,
        "^:" => Token::Colon, "^," => Token::Comma,
        r"^\$" => Token::Dollar, r"^\*" => Token::Star,
        "^(#|;|//).*$" => Token::Comment,
    ];

    static ref MNEMONIC_WITHOUT_SIZE: Regex = {
        let mut regex = String::from("^(?P<mnemonic>");
        let mut first = true;
        for mnemonic in mnemonics_without_size() {
            if first {
                first = false;
            } else {
                regex += "|";
            }
            regex += mnemonic;
        }
        regex += r")\b";
        Regex::new(regex.as_str()).unwrap()
    };

    static ref MNEMONIC_WITH_SIZE: Regex = {
        let mut regex = String::from("^(?P<mnemonic>");
        let mut first = true;
        for mnemonic in mnemonics_with_size() {
            if first {
                first = false;
            } else {
                regex += "|";
            }
            regex += mnemonic;
        }
        regex += r")(?P<size>[bwl]?)\b";
        Regex::new(regex.as_str()).unwrap()
    };

    static ref REGISTER: Regex = {
        let mut regex = String::from(r"^%(?P<name>");
        let mut first = true;
        for register in registers() {
            if first {
                first = false;
            } else {
                regex += "|";
            }
            regex += register;
        }
        regex += ")";
        Regex::new(regex.as_str()).unwrap()
    };

    static ref INTEGER: Regex = Regex::new(
        "(?P<bin>^0b[01]+)|(?P<hex>^0x[0-9a-f]+)|(?P<dec>^[1-9][0-9]*)|(?P<oct>^0[0-7]*)|(?P<char>^'[ -~]')"
    ).unwrap();

    static ref STRING: Regex = Regex::new(
        r#"^"[ -~]*""#
    ).unwrap();

    static ref SYMBOL: Regex = Regex::new(
        "^[a-zA-Z_.][a-zA-Z0-9_.]*"
    ).unwrap();
}

impl Token {

    fn match_integer(str: &str) -> Option<(Token, usize)> {
        if let Some(cap) = INTEGER.captures(str) {
            if let Some(bin) = cap.name("bin") {
                let end = bin.end();
                return match u32::from_str_radix(&str[2..end], 2) {
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some((Token::Err(Error::ParseIntFail), 0)), // 有可能过大无法转换
                };
            }
            if let Some(oct) = cap.name("oct") {
                let end = oct.end();
                return match u32::from_str_radix(&str[0..end], 8) { // 0 被归到 8 进制
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some((Token::Err(Error::ParseIntFail), 0)),
                };
            }
            if let Some(hex) = cap.name("hex") {
                let end = hex.end();
                return match u32::from_str_radix(&str[2..end], 16) {
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some((Token::Err(Error::ParseIntFail), 0)),
                };
            }
            if let Some(dec) = cap.name("dec") {
                let end = dec.end();
                return match u32::from_str_radix(&str[0..end], 10) {
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some((Token::Err(Error::ParseIntFail), 0)),
                };
            }
            if let Some(_) = cap.name("char") {
                return Some((Token::Integer(str.bytes().nth(1).unwrap() as u32), 3));
            }
        }
        None
    }

    /// 返回从 str 中匹配到的第一个符号, 符号前空白字符的长度, 符号的长度, 匹配失败则返回 Token::Err
    fn matching(str: &str) -> (Token, usize, usize) {
        let len = str.len();
        let str = str.trim_start();
        let trim_len = len - str.len();
        // pseudos
        for (reg, token) in &*PSEUDOS {
            if let Some(m) = reg.find(str) {
                return (token.clone(), trim_len, m.end());
            }
        }
        // simple instruction
        if let Some(cap) = MNEMONIC_WITHOUT_SIZE.captures(str) {
            let end = cap.get(0).unwrap().end();
            return (
                Token::Mnemonic(String::from(cap.name("mnemonic").unwrap().as_str()), None),
                trim_len,
                end
            );
        }
        // instructions with size
        if let Some(cap) = MNEMONIC_WITH_SIZE.captures(str) {
            let end = cap.get(0).unwrap().end();
            let mnemonic = cap.name("mnemonic").unwrap().as_str();
            let size = match cap.name("size").unwrap().as_str() {
                "b" => Some(Size::Byte),
                "w" => Some(Size::Word),
                "l" => Some(Size::DoubleWord),
                _ => None,
            };
            return (
                Token::Mnemonic(String::from(mnemonic), size),
                trim_len,
                end
            );
        }
        // registers
        if let Some(cap) = REGISTER.captures(str) {
            let end = cap.get(0).unwrap().end();
            return (
                Token::Register(String::from(cap.name("name").unwrap().as_str())),
                trim_len,
                end
            );
        }
        // integer
        if let Some((token, size)) = Self::match_integer(str) {
            return (token, trim_len, size);
        }
        // string
        if let Some(m) = STRING.find(str) {
            let end = m.end();
            return  (Token::String(String::from(&str[1..(end - 1)])), trim_len, end);
        }
        // symbol
        if let Some(m) = SYMBOL.find(str) {
            return (Token::Symbol(String::from(m.as_str())), trim_len, m.end());
        }
        // err
        let mut err_symbol_len = 0;
        for c in str.chars() {
            if c.is_whitespace() {
                break;
            }
            err_symbol_len += 1;
        }
        (Token::Err(Error::UnknownSymbol), trim_len, err_symbol_len)
    }
}

pub(super) struct Scanner<R: BufRead> {
    reader: R,
    buffer: String,  // 缓存一行
    cursor: usize,  // 指示分析到 buffer 的位置
    line: usize, // 当前行号, 初始为 0, 从 1 开始
    last_cursor: usize, // 上一个符号的 cursor
    last_size: usize, // 上一个符号的大小
    end: bool, // 是否已经结束, 结束后再调用 next 将返回 None
}

impl<R: BufRead> Scanner<R> {

    pub(super) fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: String::new(),
            cursor: 0,
            line: 0,
            last_cursor: 0,
            last_size: 0,
            end: false,
        }
    }

    /// 返回上个符号的行号, 列号, 内容(行号列号均从 1 开始), 其中 Eol 的行号为新行, 列号 0, 内容为空串
    pub(super) fn last_token_info(&self) -> (usize, usize, &str) {
        (
            self.line,
            self.last_cursor + 1,
            &self.buffer[self.last_cursor..(self.last_cursor + self.last_size)]
        )
    }

    fn read_line(&mut self) {
        self.buffer.clear();
        self.cursor = 0;
        self.last_cursor = 0;
        self.last_size = 0;
        let size = self.reader.read_line(&mut self.buffer).unwrap();
        self.line += 1;
        if self.buffer.ends_with("\n") {
            self.buffer.pop();
        }
        if size == 0 {
            self.end = true;
        }
    }

}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = Token; // 返回除了 Comment 之外的任何符号

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        }
        if self.line == 0 { // 还未开始读取
            self.read_line(); // 这里有可能没有内容直接结尾, 但是没有关系
        }
        if self.cursor >= self.buffer.len() { // 一行已经匹配结束
            self.read_line();
            return Some(Token::Eol); // 每行结尾会插入一个 Eol
        }
        let str = &self.buffer[self.cursor..];
        let (token, trim_len, size) = Token::matching(str);
        self.last_cursor = self.cursor + trim_len;
        self.last_size = size;
        self.cursor +=  trim_len + size;
        if token == Token::Comment {
            self.next()
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::common::{Size, Error};

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
                Token::Eol,
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
                Token::Eol,
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
                Token::Eol,
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
                Token::Eol,
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
            DotByte, Integer(20), Comma, Integer(22), Token::Eol,
            DotWord, Integer(300), Token::Eol,
            DotWord, Integer(300), Token::Eol,
            DotLong, Integer(70000), Token::Eol,
            DotLong, Integer(70000), Token::Eol,
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
            Token::DotAsciz, Token::String(String::from("hello")), Token::Eol,
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
                Token::Eol,
            ]
        );
    }

    #[test]
    fn test_comment() {
        use Token::*;
        let str = r".byte 20, 22, 'h' # a byte
                        .word 300 # a word
                        .short 300 ; a word
                        .long 70000 // a long (double word)
                        .int 70000";
        let tokens = vec![
            DotByte, Integer(20), Comma, Integer(22), Comma, Integer('h' as u32), Token::Eol,
            DotWord, Integer(300), Token::Eol,
            DotWord, Integer(300), Token::Eol,
            DotLong, Integer(70000), Token::Eol,
            DotLong, Integer(70000), Token::Eol,
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
            Token::DotLcomm, Token::Symbol(String::from("buffer")), Token::Comma, Token::Integer(16), Token::Eol,
        ];
        test_str(str, tokens);
    }

    #[test]
    fn test_simple_instruction() {
        test_str(
            "main:\npusha\npushf\nint $0x80\njmp *0x1234",
            vec![
                Token::Symbol(String::from("main")), Token::Colon, Token::Eol,
                Token::Mnemonic(String::from("pusha"), None), Token::Eol,
                Token::Mnemonic(String::from("pushf"), None), Token::Eol,
                Token::Mnemonic(String::from("int"), None), Token::Dollar,
                Token::Integer(0x80), Token::Eol,
                Token::Mnemonic(String::from("jmp"), None), Token::Star,
                Token::Integer(0x1234), Token::Eol,
            ]
        );
    }

    #[test]
    fn test_instruction_with_size() {
        test_str(
            "pushb $0x10\npush %eax\nmovl %ebx, %cs:0x01(%eax, %ebx, 2)",
            vec![
                Token::Mnemonic(String::from("push"), Some(Size::Byte)), Token::Dollar, Token::Integer(0x10), Token::Eol,
                Token::Mnemonic(String::from("push"), None), Token::Register(String::from("eax")), Token::Eol,
                Token::Mnemonic(String::from("mov"), Some(Size::DoubleWord)),
                Token::Register(String::from("ebx")),Token::Comma,
                Token::Register(String::from("cs")), Token::Colon, Token::Integer(0x01),
                Token::Lparen, Token::Register(String::from("eax")), Token::Comma,
                Token::Register(String::from("ebx")), Token::Comma, Token::Integer(2),
                Token::Rparen,
                Token::Eol,
            ]
        );
    }

    #[test]
    fn test_last_token_info() {
        let cursor = Cursor::new(".section .text\nmovl %eax, %ecx");
        let mut scanner = Scanner::new(cursor);

        assert_eq!(scanner.next(), Some(Token::DotSection));
        assert_eq!(scanner.last_token_info(), (1, 1, ".section"));

        assert_eq!(scanner.next(), Some(Token::Symbol(String::from(".text"))));
        assert_eq!(scanner.last_token_info(), (1, 10, ".text"));

        assert_eq!(scanner.next(), Some(Token::Eol));

        assert_eq!(scanner.next(), Some(Token::Mnemonic(String::from("mov"), Some(Size::DoubleWord))));
        assert_eq!(scanner.last_token_info(), (2, 1, "movl"));

        assert_eq!(scanner.next(), Some(Token::Register(String::from("eax"))));
        assert_eq!(scanner.last_token_info(), (2, 6, "%eax"));

        assert_eq!(scanner.next(), Some(Token::Comma));
        assert_eq!(scanner.last_token_info(), (2, 10, ","));

        assert_eq!(scanner.next(), Some(Token::Register(String::from("ecx"))));
        assert_eq!(scanner.last_token_info(), (2, 12, "%ecx"));

        assert_eq!(scanner.next(), Some(Token::Eol));

        assert_eq!(scanner.next(), None);
    }

    #[test]
    fn test_err() {
        let cursor = Cursor::new("%abc");
        let mut scanner = Scanner::new(cursor);

        assert_eq!(scanner.next(), Some(Token::Err(Error::UnknownSymbol)));
        assert_eq!(scanner.last_token_info(), (1, 1, "%abc"));

        assert_eq!(scanner.next(), Some(Token::Eol));
        assert_eq!(scanner.next(), None);
    }

}