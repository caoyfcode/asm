use std::io::BufRead;

use lazy_static::lazy_static;
use regex::Regex;

use crate::common::{Size, Error};
use crate::config::{mnemonics_without_size, mnemonics_with_size, registers};

#[derive(Debug, PartialEq, Clone)]
pub(super) enum TokenKind {
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

#[derive(Debug, PartialEq)]
pub(super) struct Token {
    pub kind: TokenKind,
    pub line: usize, // 行号, 从 1 开始
    pub content: String,
}

impl Token {
    fn new(kind: TokenKind, line: usize, content: String) -> Self {
        Self { kind, line, content }
    }
}

macro_rules! regex_token_vec {
    ( $($r:expr => $t:expr),* $(,)? ) => {
        vec![
            $( (Regex::new($r).unwrap(), $t) ),*
        ]
    };
}

lazy_static! {
    static ref SIMPLE_TOKENS: Vec<(Regex, TokenKind)> = regex_token_vec![
        r"^\.section\b" => TokenKind::DotSection,
        r"^\.(global|globl)\b" => TokenKind::DotGlobal,
        r"^\.(equ|set)\b" => TokenKind::DotEqu,
        r"^\.fill\b" => TokenKind::DotFill,
        r"^\.byte\b" => TokenKind::DotByte,
        r"^\.(word|short)\b" => TokenKind::DotWord,
        r"^\.(long|int)\b" => TokenKind::DotLong,
        r"^\.ascii\b" => TokenKind::DotAscii,
        r"^\.(asciz|string)\b" => TokenKind::DotAsciz,
        r"^\.comm\b" => TokenKind::DotComm,
        r"^\.lcomm\b" => TokenKind::DotLcomm,
        r"^\(" => TokenKind::Lparen, r"^\)" => TokenKind::Rparen,
        "^:" => TokenKind::Colon, "^," => TokenKind::Comma,
        r"^\$" => TokenKind::Dollar, r"^\*" => TokenKind::Star,
        "^(#|;|//).*$" => TokenKind::Comment,
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

pub(super) struct Scanner<R: BufRead> {
    reader: R,
    buffer: String,  // 缓存一行
    cursor: usize,  // 指示分析到 buffer 的位置
    line: usize, // 当前行号, 初始为 0, 从 1 开始
    end: bool, // 是否已经结束, 结束后再调用 next 将返回 None
}

impl<R: BufRead> Scanner<R> {

    pub(super) fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: String::new(),
            cursor: 0,
            line: 0,
            end: false,
        }
    }

    fn read_line(&mut self) {
        self.buffer.clear();
        self.cursor = 0;
        let size = self.reader.read_line(&mut self.buffer).unwrap();
        self.line += 1;
        if self.buffer.ends_with("\n") {
            self.buffer.pop();
        }
        if size == 0 {
            self.end = true;
        }
    }

    /// 从 cursor 位置匹配一个整数, 返回符号与其长度
    fn match_integer(&self) -> Option<(TokenKind, usize)> {
        let str = &self.buffer[self.cursor..];
        if let Some(cap) = INTEGER.captures(str) {
            if let Some(bin) = cap.name("bin") {
                let end = bin.end();
                return match u32::from_str_radix(&str[2..end], 2) {
                    Ok(integer) => Some((TokenKind::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        TokenKind::Err(Error::ParseIntFail(self.line, String::from(bin.as_str()))),
                        end
                    )),
                };
            }
            if let Some(oct) = cap.name("oct") {
                let end = oct.end();
                return match u32::from_str_radix(&str[0..end], 8) { // 0 被归到 8 进制
                    Ok(integer) => Some((TokenKind::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        TokenKind::Err(Error::ParseIntFail(self.line, String::from(oct.as_str()))),
                        end
                    )),
                };
            }
            if let Some(hex) = cap.name("hex") {
                let end = hex.end();
                return match u32::from_str_radix(&str[2..end], 16) {
                    Ok(integer) => Some((TokenKind::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        TokenKind::Err(Error::ParseIntFail(self.line, String::from(hex.as_str()))),
                        end
                    )),
                };
            }
            if let Some(dec) = cap.name("dec") {
                let end = dec.end();
                return match u32::from_str_radix(&str[0..end], 10) {
                    Ok(integer) => Some((TokenKind::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        TokenKind::Err(Error::ParseIntFail(self.line, String::from(dec.as_str()))),
                        end
                    )),
                };
            }
            if let Some(_) = cap.name("char") {
                return Some((TokenKind::Integer(str.bytes().nth(1).unwrap() as u32), 3));
            }
        }
        None
    }

    /// 从 cursor 位置匹配一个符号, 返回符号与其长度
    fn matching(&self) -> (TokenKind, usize) {
        let str = &self.buffer[self.cursor..];
        // pseudos
        for (reg, token) in &*SIMPLE_TOKENS {
            if let Some(m) = reg.find(str) {
                return (token.clone(), m.end());
            }
        }
        // simple instruction
        if let Some(cap) = MNEMONIC_WITHOUT_SIZE.captures(str) {
            let end = cap.get(0).unwrap().end();
            return (
                TokenKind::Mnemonic(String::from(cap.name("mnemonic").unwrap().as_str()), None),
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
                TokenKind::Mnemonic(String::from(mnemonic), size),
                end
            );
        }
        // registers
        if let Some(cap) = REGISTER.captures(str) {
            let end = cap.get(0).unwrap().end();
            return (
                TokenKind::Register(String::from(cap.name("name").unwrap().as_str())),
                end
            );
        }
        // integer
        if let Some((token, size)) = self.match_integer() {
            return (token, size);
        }
        // string
        if let Some(m) = STRING.find(str) {
            let end = m.end();
            return  (TokenKind::String(String::from(&str[1..(end - 1)])), end);
        }
        // symbol
        if let Some(m) = SYMBOL.find(str) {
            return (TokenKind::Symbol(String::from(m.as_str())), m.end());
        }
        // err
        let mut err_symbol_len = 0;
        for c in str.chars() {
            if c.is_whitespace() {
                break;
            }
            err_symbol_len += 1;
        }
        (TokenKind::Err(Error::UnknownSymbol(self.line, String::from(&str[..err_symbol_len]))), err_symbol_len)
    }

}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = Token; // 符号(除了Comment), 行号, 符号内容

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        }
        if self.line == 0 { // 还未开始读取
            self.read_line();
        }
        if self.cursor >= self.buffer.len() { // 一行已经匹配结束
            let token = Token::new(TokenKind::Eol, self.line, String::new());
            self.read_line();
            return Some(token); // 每行结尾会插入一个 Eol
        }
        let str = &self.buffer[self.cursor..];
        let trim_str = str.trim_start();
        self.cursor += str.len() - trim_str.len();
        if self.cursor >= self.buffer.len() {
            return self.next();
        }
        let (token, size) = self.matching();
        let content = String::from(&self.buffer[self.cursor..self.cursor + size]);
        self.cursor +=  size;
        if token == TokenKind::Comment {
            self.next()
        } else {
            Some(Token::new(token, self.line, content))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::{common::{Size, Error}, parser::scanner::Token};

    use super::{Scanner, TokenKind};

    fn test_str_token_kinds(str: &str, tokens: Vec<TokenKind>) {
        let cursor = Cursor::new(str);
        let scanner = Scanner::new(cursor);

        let mut iter = tokens.into_iter();
        for Token {kind, ..} in scanner {
            assert_eq!(Some(kind), iter.next())
        }
        assert_eq!(None, iter.next());
    }

    fn test_str_tokens(str: &str, items: Vec<Token>) {
        let cursor = Cursor::new(str);
        let scanner = Scanner::new(cursor);

        let mut iter = items.into_iter();
        for item in scanner {
            assert_eq!(Some(item), iter.next());
        }
        assert_eq!(None, iter.next());
    }

    #[test]
    fn test_section_symbol() {
        test_str_token_kinds(
            ".section .text\n.section .data",
            vec![
                TokenKind::DotSection,
                TokenKind::Symbol(String::from(".text")),
                TokenKind::Eol,
                TokenKind::DotSection,
                TokenKind::Symbol(String::from(".data")),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_global_symbol() {
        test_str_token_kinds(
            ".global _start\n.globl main",
            vec![
                TokenKind::DotGlobal,
                TokenKind::Symbol(String::from("_start")),
                TokenKind::Eol,
                TokenKind::DotGlobal,
                TokenKind::Symbol(String::from("main")),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_equ_symbol_comma_value() {
        test_str_token_kinds(
            ".equ len, 3\n.set a, len",
            vec![
                TokenKind::DotEqu,
                TokenKind::Symbol(String::from("len")),
                TokenKind::Comma,
                TokenKind::Integer(3),
                TokenKind::Eol,
                TokenKind::DotEqu,
                TokenKind::Symbol(String::from("a")),
                TokenKind::Comma,
                TokenKind::Symbol(String::from("len")),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_fill_repeat_size_value() {
        test_str_token_kinds(
            ".fill 3, 2, 0",
            vec![
                TokenKind::DotFill,
                TokenKind::Integer(3),
                TokenKind::Comma,
                TokenKind::Integer(2),
                TokenKind::Comma,
                TokenKind::Integer(0),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_data_section_integers() {
        use TokenKind::*;
        let str = r".byte 20, 22
                        .word 300
                        .short 300
                        .long 70000
                        .int 70000";
        let tokens = vec![
            DotByte, Integer(20), Comma, Integer(22), TokenKind::Eol,
            DotWord, Integer(300), TokenKind::Eol,
            DotWord, Integer(300), TokenKind::Eol,
            DotLong, Integer(70000), TokenKind::Eol,
            DotLong, Integer(70000), TokenKind::Eol,
        ];

        test_str_token_kinds(str, tokens);
    }

    #[test]
    fn test_data_section_strings() {
        let str = r#".ascii "hello"
                        .asciz "hello"
                        .string "hello""#;
        let tokens = vec![
            TokenKind::DotAscii, TokenKind::String(String::from("hello")), TokenKind::Eol,
            TokenKind::DotAsciz, TokenKind::String(String::from("hello")), TokenKind::Eol,
            TokenKind::DotAsciz, TokenKind::String(String::from("hello")), TokenKind::Eol,
        ];

        test_str_token_kinds(str, tokens);
    }

    #[test]
    fn test_bss_section_decalarations() {
        test_str_token_kinds(
            ".comm a, 4\n.lcomm b, 8",
            vec![
                TokenKind::DotComm,
                TokenKind::Symbol(String::from("a")),
                TokenKind::Comma,
                TokenKind::Integer(4),
                TokenKind::Eol,
                TokenKind::DotLcomm,
                TokenKind::Symbol(String::from("b")),
                TokenKind::Comma,
                TokenKind::Integer(8),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_comment() {
        use TokenKind::*;
        let str = r".byte 20, 22, 'h' # a byte
                        .word 300 # a word
                        .short 300 ; a word
                        .long 70000 // a long (double word)
                        .int 70000";
        let tokens = vec![
            DotByte, Integer(20), Comma, Integer(22), Comma, Integer('h' as u32), TokenKind::Eol,
            DotWord, Integer(300), TokenKind::Eol,
            DotWord, Integer(300), TokenKind::Eol,
            DotLong, Integer(70000), TokenKind::Eol,
            DotLong, Integer(70000), TokenKind::Eol,
        ];

        test_str_token_kinds(str, tokens);
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
            TokenKind::DotSection, TokenKind::Symbol(String::from(".text")), TokenKind::Eol,
            TokenKind::DotGlobal, TokenKind::Symbol(String::from("main")), TokenKind::Eol,
            TokenKind::Symbol(String::from("main")), TokenKind::Colon, TokenKind::Eol,
            TokenKind::DotSection, TokenKind::Symbol(String::from(".data")), TokenKind::Eol,
            TokenKind::Symbol(String::from("hello")), TokenKind::Colon,
            TokenKind::DotAsciz, TokenKind::String(String::from("Hello, World")), TokenKind::Eol,
            TokenKind::Symbol(String::from("num")), TokenKind::Colon,
            TokenKind::DotLong, TokenKind::Integer(0xf0), TokenKind::Eol,
            TokenKind::DotEqu, TokenKind::Symbol(String::from("haha")), TokenKind::Comma, TokenKind::Integer(0b1101), TokenKind::Eol,
            TokenKind::DotSection, TokenKind::Symbol(String::from(".bss")), TokenKind::Eol,
            TokenKind::DotLcomm, TokenKind::Symbol(String::from("buffer")), TokenKind::Comma, TokenKind::Integer(16), TokenKind::Eol,
        ];
        test_str_token_kinds(str, tokens);
    }

    #[test]
    fn test_simple_instruction() {
        test_str_token_kinds(
            "main:\npusha\npushf\nint $0x80\njmp *0x1234",
            vec![
                TokenKind::Symbol(String::from("main")), TokenKind::Colon, TokenKind::Eol,
                TokenKind::Mnemonic(String::from("pusha"), None), TokenKind::Eol,
                TokenKind::Mnemonic(String::from("pushf"), None), TokenKind::Eol,
                TokenKind::Mnemonic(String::from("int"), None), TokenKind::Dollar,
                TokenKind::Integer(0x80), TokenKind::Eol,
                TokenKind::Mnemonic(String::from("jmp"), None), TokenKind::Star,
                TokenKind::Integer(0x1234), TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_instruction_with_size() {
        test_str_token_kinds(
            "pushb $0x10\npush %eax\nmovl %ebx, %cs:0x01(%eax, %ebx, 2)",
            vec![
                TokenKind::Mnemonic(String::from("push"), Some(Size::Byte)), TokenKind::Dollar, TokenKind::Integer(0x10), TokenKind::Eol,
                TokenKind::Mnemonic(String::from("push"), None), TokenKind::Register(String::from("eax")), TokenKind::Eol,
                TokenKind::Mnemonic(String::from("mov"), Some(Size::DoubleWord)),
                TokenKind::Register(String::from("ebx")),TokenKind::Comma,
                TokenKind::Register(String::from("cs")), TokenKind::Colon, TokenKind::Integer(0x01),
                TokenKind::Lparen, TokenKind::Register(String::from("eax")), TokenKind::Comma,
                TokenKind::Register(String::from("ebx")), TokenKind::Comma, TokenKind::Integer(2),
                TokenKind::Rparen,
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_token_kind_and_info() {
        test_str_tokens(
            ".section .text\nmovl %eax, %ecx",
            vec![
                Token::new(TokenKind::DotSection, 1, String::from(".section")),
                Token::new(TokenKind::Symbol(String::from(".text")), 1, String::from(".text")),
                Token::new(TokenKind::Eol, 1, String::from("")),
                Token::new(TokenKind::Mnemonic(String::from("mov"), Some(Size::DoubleWord)), 2, String::from("movl")),
                Token::new(TokenKind::Register(String::from("eax")), 2, String::from("%eax")),
                Token::new(TokenKind::Comma, 2, String::from(",")),
                Token::new(TokenKind::Register(String::from("ecx")), 2, String::from("%ecx")),
                Token::new(TokenKind::Eol, 2, String::from("")),
            ]
        );
    }

    #[test]
    fn test_err_unknown_symbol() {
        test_str_token_kinds(
            "%abc %eax",
            vec![
                TokenKind::Err(Error::UnknownSymbol(1, String::from("%abc"))),
                TokenKind::Register(String::from("eax")),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_err_parse_int() {
        test_str_token_kinds(
            ".int 999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
            vec![
                TokenKind::DotLong,
                TokenKind::Err(Error::ParseIntFail(
                    1,
                    String::from("999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
                )),
                TokenKind::Eol,
            ]
        );
    }

}