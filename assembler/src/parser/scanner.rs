use std::io::BufRead;

use lazy_static::lazy_static;
use regex::Regex;

use crate::common::Error;

#[derive(Debug, PartialEq, Clone)]
pub(super) enum TokenKind {
    // 寄存器
    Register(String), // String 储存不带 % 的寄存器名(小写)
    // 操作符
    Lparen, Rparen, Colon, Comma, Dollar, Star,
    // 数据
    Integer(u32),
    String(String),
    // 标识符
    Label(String),
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
    pub content: String, // 用于打印错误信息
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
        r"^\(" => TokenKind::Lparen, r"^\)" => TokenKind::Rparen,
        "^:" => TokenKind::Colon, "^," => TokenKind::Comma,
        r"^\$" => TokenKind::Dollar, r"^\*" => TokenKind::Star,
        "^(#|;|//).*$" => TokenKind::Comment,
    ];

    static ref REGISTER: Regex = Regex::new(
        "^%[a-zA-Z]+",
    ).unwrap();

    static ref INTEGER: Regex = Regex::new(
        "(?P<bin>^0b[01]+)|(?P<hex>^0x[0-9a-f]+)|(?P<dec>^[1-9][0-9]*)|(?P<oct>^0[0-7]*)|(?P<char>^'[ -~]')"
    ).unwrap();

    static ref STRING: Regex = Regex::new(
        r#"^"[ -~]*""#
    ).unwrap();

    static ref LABEL: Regex = Regex::new(
        "^[a-zA-Z_.][a-zA-Z0-9_.]*:"
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
        // simple
        for (reg, token) in &*SIMPLE_TOKENS {
            if let Some(m) = reg.find(str) {
                return (token.clone(), m.end());
            }
        }
        // registers
        if let Some(m) = REGISTER.find(str) {
            let end = m.end();
            return (TokenKind::Register(String::from(&str[1..end])), m.end());
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
        // label
        if let Some(m) = LABEL.find(str) {
            let end = m.end();
            return (TokenKind::Label(String::from(&str[..(end - 1)])), end);
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

    use crate::common::Error;

    use super::{Scanner, TokenKind, Token};

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
    fn test_simple() {
        test_str_token_kinds(
            r#".section .text
                main: # main function
                    pushl %cs:0x01(%eax)
                    mov $0x1, %eax
                    jmp *%eax"#,
            vec![
                TokenKind::Symbol(String::from(".section")), TokenKind::Symbol(String::from(".text")), TokenKind::Eol,
                TokenKind::Label(String::from("main")), TokenKind::Eol,
                TokenKind::Symbol(String::from("pushl")), TokenKind::Register(String::from("cs")),
                TokenKind::Colon, TokenKind::Integer(0x01),
                TokenKind::Lparen, TokenKind::Register(String::from("eax")), TokenKind::Rparen, TokenKind::Eol,
                TokenKind::Symbol(String::from("mov")), TokenKind::Dollar, TokenKind::Integer(0x1),
                TokenKind::Comma, TokenKind::Register(String::from("eax")), TokenKind::Eol,
                TokenKind::Symbol(String::from("jmp")), TokenKind::Star, TokenKind::Register(String::from("eax")),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_string() {
        test_str_token_kinds(
            r#".section .data ; data section
                hello:
                    .string "Hello, World""#,
            vec![
                TokenKind::Symbol(String::from(".section")), TokenKind::Symbol(String::from(".data")), TokenKind::Eol,
                TokenKind::Label(String::from("hello")), TokenKind::Eol,
                TokenKind::Symbol(String::from(".string")), TokenKind::String(String::from("Hello, World")), TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_integer() {
        test_str_token_kinds(
            r#".section .data ; data section
                num:
                    .int 0xf0, 0b1101, 0701
                    .byte 20, 22, 'h'"#,
            vec![
                TokenKind::Symbol(String::from(".section")), TokenKind::Symbol(String::from(".data")), TokenKind::Eol,
                TokenKind::Label(String::from("num")), TokenKind::Eol,
                TokenKind::Symbol(String::from(".int")), TokenKind::Integer(0xf0),
                TokenKind::Comma, TokenKind::Integer(0b1101),
                TokenKind::Comma, TokenKind::Integer(0o701), TokenKind::Eol,
                TokenKind::Symbol(String::from(".byte")), TokenKind::Integer(20),
                TokenKind::Comma, TokenKind::Integer(22),
                TokenKind::Comma, TokenKind::Integer('h' as u32), TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_err_parse_int() {
        test_str_token_kinds(
            "999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
            vec![
                TokenKind::Err(Error::ParseIntFail(
                    1,
                    String::from("999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
                )),
                TokenKind::Eol,
            ]
        );
    }

    #[test]
    fn test_err_unknown_symbol() {
        test_str_token_kinds(
            "@",
            vec![
                TokenKind::Err(Error::UnknownSymbol(1, String::from("@"))),
                TokenKind::Eol,
            ]
        );
    }


    #[test]
    fn test_token_kind_and_info() {
        test_str_tokens(
            ".section .text\nmovl %eax, %ecx",
            vec![
                Token::new(TokenKind::Symbol(String::from(".section")), 1, String::from(".section")),
                Token::new(TokenKind::Symbol(String::from(".text")), 1, String::from(".text")),
                Token::new(TokenKind::Eol, 1, String::from("")),
                Token::new(TokenKind::Symbol(String::from("movl")), 2, String::from("movl")),
                Token::new(TokenKind::Register(String::from("eax")), 2, String::from("%eax")),
                Token::new(TokenKind::Comma, 2, String::from(",")),
                Token::new(TokenKind::Register(String::from("ecx")), 2, String::from("%ecx")),
                Token::new(TokenKind::Eol, 2, String::from("")),

            ]
        );
    }

}