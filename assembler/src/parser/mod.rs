use std::{io::BufRead, collections::VecDeque};

use crate::{ast::{Ast, ProgramNode, ProgramItem, InstructionNode, LabelNode, PseudoSectionNode, PseudoGlobalNode, PseudoEquNode, PseudoFillNode, PseudoIntegerNode, PseudoStringNode, PseudoCommNode, ValueNode}, common::Size};

use self::scanner::{Scanner, Token};

mod scanner;

/// 对一个 `Option<Token>` 进行模式匹配, 匹配特定模式的 `Token`, 并可以根据需要返回
///
/// 参数为:
/// - 进行匹配的 `Option<Token>` 或引用类型
/// - 要匹配的模式的信息, 用于匹配失败 panic 信息, 类型需要实现 `Display`
/// - 逗号分隔的模式列表, 若要返回值, 使用 `<pat> => <expr>` 形式, 不返回值则使用 `<pat>`
macro_rules! match_token {
    ($t:expr, $pat_msg:expr, $($pat:pat => $out:expr),+ $(,)?) => {
        match $t {
            $(Some($pat) => $out,)+
            Some(other) => panic!("syntax error: expected {}, but found {:?}", $pat_msg, other),
            None => panic!("syntax error: expected {}, but found nothing", $pat_msg),
        }
    };
    ($t:expr, $pat_msg:expr, $($pat:pat),+ $(,)?) => {
        match $t {
            $(Some($pat) => (),)+
            Some(other) => panic!("syntax error: expected {}, but found {:?}", $pat_msg, other),
            None => panic!("syntax error: expected {}, but found nothing", $pat_msg),
        }
    };
}

pub struct Parser<R: BufRead> {
    scanner: Scanner<R>,
    lookahead_buffer: VecDeque<Token>,
}

impl<R: BufRead> Parser<R> {
    pub fn new(reader: R) -> Self {
        Self {
            scanner: Scanner::new(reader),
            lookahead_buffer: VecDeque::new(),
        }
    }

    pub fn build_ast(&mut self) -> Ast {
        Ast::new(self.program())
    }

    fn lookahead_nth(&mut self, n: usize) -> Option<&Token> {
        while self.lookahead_buffer.len() <= n {
            let token = self.scanner.next()?;
            self.lookahead_buffer.push_back(token);
        }
        Some(&self.lookahead_buffer[n])
    }

    fn lookahead(&mut self) -> Option<&Token> {
        self.lookahead_nth(0)
    }

    fn next_token(&mut self) -> Option<Token> {
        let token = if self.lookahead_buffer.len() < 1 {
            self.scanner.next()
        } else {
            self.lookahead_buffer.pop_front()
        };
        token
    }

    fn program(&mut self) -> ProgramNode {
        let mut items = Vec::new();

        while self.lookahead().is_some() {
            let mut match_eol = true;
            match_token! {
                self.lookahead(), "pseudo or mnemonic or symbol or eol",
                Token::DotSection => items.push(ProgramItem::PseudoSection(self.pseudo_section())),
                Token::DotGlobal => items.push(ProgramItem::PseudoGlobal(self.pseudo_global())),
                Token::DotEqu => items.push(ProgramItem::PseudoEqu(self.pseudo_equ())),
                Token::DotFill => items.push(ProgramItem::PseudoFill(self.pseudo_fill())),
                Token::DotByte | Token::DotWord | Token::DotLong => items.push(ProgramItem::PseudoInteger(self.pseudo_integer())),
                Token::DotAscii | Token::DotAsciz=> items.push(ProgramItem::PseudoString(self.pseudo_string())),
                Token::DotComm | Token::DotLcomm => items.push(ProgramItem::PseudoComm(self.pseudo_comm())),
                Token::Mnemonic(_, _) => items.push(ProgramItem::Instruction(self.instruction())),
                Token::Symbol(_) => {
                    items.push(ProgramItem::Label(self.label()));
                    match_eol = false;
                },
                Token::Eol => {
                    self.next_token();
                    match_eol = false;
                },
            }

            if match_eol && self.lookahead().is_some() {
                match_token! { self.next_token(), "eol", Token::Eol}
            }
        }

        ProgramNode { items }
    }

    fn pseudo_section(&mut self) -> PseudoSectionNode {
        match_token! { self.next_token(), ".section", Token::DotSection }
        match_token! {
            self.next_token(), "symbol",
            Token::Symbol(symbol) => PseudoSectionNode { symbol },
        }
    }

    fn pseudo_global(&mut self) -> PseudoGlobalNode {
        match_token! { self.next_token(), ".global/.globl", Token::DotGlobal }
        match_token! {
            self.next_token(), "symbol",
            Token::Symbol(symbol) => PseudoGlobalNode { symbol },
        }
    }

    fn pseudo_equ(&mut self) -> PseudoEquNode {
        match_token! { self.next_token(), ".equ/.set",  Token::DotEqu }
        let symbol = match_token! {
            self.next_token(), "symbol",
            Token::Symbol(symbol) => symbol,
        };
        match_token! { self.next_token(), "comma", Token::Comma }
        let value = match_token! {
            self.next_token(), "integer",
            Token::Integer(value) => value,
        };
        PseudoEquNode { symbol, value }
    }

    fn pseudo_fill(&mut self) -> PseudoFillNode {
        match_token! {self.next_token(), ".fill", Token::DotFill}
        let repeat = match_token! {
            self.next_token(), "integer",
            Token::Integer(value) => value,
        };
        match_token! { self.next_token(), "comma", Token::Comma }
        let size = match_token! {
            self.next_token(), "integer",
            Token::Integer(value) => value,
        };
        match_token! { self.next_token(), "comma", Token::Comma }
        let value = self.value();

        PseudoFillNode { repeat, size, value }
    }

    fn value(&mut self) -> ValueNode {
        match_token! {
            self.next_token(), "symbol or integer",
            Token::Integer(value) => ValueNode::Integer(value),
            Token::Symbol(symbol) => ValueNode::Symbol(symbol),
        }
    }

    fn pseudo_integer(&mut self) -> PseudoIntegerNode {
        let size = match_token! {
            self.next_token(), ".byte or .word/.short or .long/.int",
            Token::DotByte => Size::Byte,
            Token::DotWord => Size::Word,
            Token::DotLong => Size::DoubleWord,
        };
        let mut values = Vec::new();
        let mut end = false;
        while !end {
            values.push(self.value());
            end = match_token! {
                self.lookahead(), "comma or eol",
                Token::Comma => {
                    self.next_token();
                    false
                },
                Token::Eol => true
            }
        }
        PseudoIntegerNode { size, values }
    }

    fn pseudo_string(&mut self) -> PseudoStringNode {
        let zero_end = match_token! {
            self.next_token(), ".ascii or .asciz/.string",
            Token::DotAscii => false,
            Token::DotAsciz => true,
        };
        let content = match_token! {
            self.next_token(), "string",
            Token::String(str) => str,
        };
        PseudoStringNode { zero_end, content }
    }

    fn pseudo_comm(&mut self) -> PseudoCommNode {
        let is_local = match_token! {
            self.next_token(), ".lcomm or .comm",
            Token::DotLcomm => true,
            Token::DotComm => false,
        };
        let symbol = match_token! {
            self.next_token(), "symbol",
            Token::Symbol(symbol) => symbol,
        };
        match_token! { self.next_token(), "comma", Token::Comma}
        let length = self.value();
        PseudoCommNode { is_local, symbol, length }
    }

    fn instruction(&mut self) -> InstructionNode {
        todo!()
    }

    fn label(&mut self) -> LabelNode {
        let label = match_token! {
            self.next_token(), "symbol",
            Token::Symbol(symbol) => symbol,
        };
        match_token! { self.next_token(), "colon", Token::Colon }
        LabelNode { label }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::Parser;
    use crate::ast::printer::AstPrinter;


    #[test]
    fn test_pseudo() {
        let code = r#"
        .section .text
        .global main
        main: # main function
        .section .data ; data section
        hello: .string "Hello, World"
        num: .int 0xf0, 10, 'a'
        arr: .fill 5, 1, 3
        .equ haha, 0b1101
        .section .bss
        .lcomm buffer, 16
        "#.trim();
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        let parser_ast = parser.build_ast();
        let parser_ast2 = parser.build_ast();

        let mut printer = AstPrinter::new();
        parser_ast.run_visitor(&mut printer);
        println!("");
        println!("{}", printer.as_str());
    }

    #[test]
    #[should_panic]
    fn test_syntex_err_eol() {
        let code = r#"
        .section .text .global main
        "#.trim();
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        parser.build_ast();
    }
}