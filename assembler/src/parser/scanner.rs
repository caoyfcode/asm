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
    fn match_integer(&self) -> Option<(Token, usize)> {
        let str = &self.buffer[self.cursor..];
        if let Some(cap) = INTEGER.captures(str) {
            if let Some(bin) = cap.name("bin") {
                let end = bin.end();
                return match u32::from_str_radix(&str[2..end], 2) {
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        Token::Err(Error::ParseIntFail(self.line, String::from(bin.as_str()))),
                        end
                    )),
                };
            }
            if let Some(oct) = cap.name("oct") {
                let end = oct.end();
                return match u32::from_str_radix(&str[0..end], 8) { // 0 被归到 8 进制
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        Token::Err(Error::ParseIntFail(self.line, String::from(oct.as_str()))),
                        end
                    )),
                };
            }
            if let Some(hex) = cap.name("hex") {
                let end = hex.end();
                return match u32::from_str_radix(&str[2..end], 16) {
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        Token::Err(Error::ParseIntFail(self.line, String::from(hex.as_str()))),
                        end
                    )),
                };
            }
            if let Some(dec) = cap.name("dec") {
                let end = dec.end();
                return match u32::from_str_radix(&str[0..end], 10) {
                    Ok(integer) => Some((Token::Integer(integer), end)),
                    Err(_) => Some(( // 有可能过大无法转换
                        Token::Err(Error::ParseIntFail(self.line, String::from(dec.as_str()))),
                        end
                    )),
                };
            }
            if let Some(_) = cap.name("char") {
                return Some((Token::Integer(str.bytes().nth(1).unwrap() as u32), 3));
            }
        }
        None
    }

    /// 从 cursor 位置匹配一个符号, 返回符号与其长度
    fn matching(&self) -> (Token, usize) {
        let str = &self.buffer[self.cursor..];
        // pseudos
        for (reg, token) in &*PSEUDOS {
            if let Some(m) = reg.find(str) {
                return (token.clone(), m.end());
            }
        }
        // simple instruction
        if let Some(cap) = MNEMONIC_WITHOUT_SIZE.captures(str) {
            let end = cap.get(0).unwrap().end();
            return (
                Token::Mnemonic(String::from(cap.name("mnemonic").unwrap().as_str()), None),
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
                end
            );
        }
        // registers
        if let Some(cap) = REGISTER.captures(str) {
            let end = cap.get(0).unwrap().end();
            return (
                Token::Register(String::from(cap.name("name").unwrap().as_str())),
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
            return  (Token::String(String::from(&str[1..(end - 1)])), end);
        }
        // symbol
        if let Some(m) = SYMBOL.find(str) {
            return (Token::Symbol(String::from(m.as_str())), m.end());
        }
        // err
        let mut err_symbol_len = 0;
        for c in str.chars() {
            if c.is_whitespace() {
                break;
            }
            err_symbol_len += 1;
        }
        (Token::Err(Error::UnknownSymbol(self.line, String::from(&str[..err_symbol_len]))), err_symbol_len)
    }

}

impl<R: BufRead> Iterator for Scanner<R> {
    type Item = (Token, usize, String); // 符号(除了Comment), 行号, 符号内容

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        }
        if self.line == 0 { // 还未开始读取
            self.read_line();
        }
        if self.cursor >= self.buffer.len() { // 一行已经匹配结束
            self.read_line();
            return Some((Token::Eol, self.line - 1, String::new())); // 每行结尾会插入一个 Eol
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
        if token == Token::Comment {
            self.next()
        } else {
            Some((token, self.line, content))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::common::{Size, Error};

    use super::{Scanner, Token};

    fn test_str_tokens(str: &str, tokens: Vec<Token>) {
        let cursor = Cursor::new(str);
        let scanner = Scanner::new(cursor);

        let mut iter = tokens.into_iter();
        for (token, ..) in scanner {
            assert_eq!(Some(token), iter.next())
        }
        assert_eq!(None, iter.next());
    }

    fn test_str_token_infos(str: &str, items: Vec<(Token, usize, String)>) {
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
        test_str_tokens(
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
        test_str_tokens(
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
        test_str_tokens(
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
        test_str_tokens(
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

        test_str_tokens(str, tokens);
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

        test_str_tokens(str, tokens);
    }

    #[test]
    fn test_bss_section_decalarations() {
        test_str_tokens(
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

        test_str_tokens(str, tokens);
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
        test_str_tokens(str, tokens);
    }

    #[test]
    fn test_simple_instruction() {
        test_str_tokens(
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
        test_str_tokens(
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
        test_str_token_infos(
            ".section .text\nmovl %eax, %ecx",
            vec![
                (Token::DotSection, 1, String::from(".section")),
                (Token::Symbol(String::from(".text")), 1, String::from(".text")),
                (Token::Eol, 1, String::from("")),
                (Token::Mnemonic(String::from("mov"), Some(Size::DoubleWord)), 2, String::from("movl")),
                (Token::Register(String::from("eax")), 2, String::from("%eax")),
                (Token::Comma, 2, String::from(",")),
                (Token::Register(String::from("ecx")), 2, String::from("%ecx")),
                (Token::Eol, 2, String::from("")),
            ]
        );
    }

    #[test]
    fn test_err_unknown_symbol() {
        test_str_tokens(
            "%abc %eax",
            vec![
                Token::Err(Error::UnknownSymbol(1, String::from("%abc"))),
                Token::Register(String::from("eax")),
                Token::Eol,
            ]
        );
    }

    #[test]
    fn test_err_parse_int() {
        test_str_tokens(
            ".int 999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
            vec![
                Token::DotLong,
                Token::Err(Error::ParseIntFail(
                    1,
                    String::from("999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
                )),
                Token::Eol,
            ]
        );
    }

}