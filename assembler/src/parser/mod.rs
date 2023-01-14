use std::io::BufRead;

use crate::{ast::{Ast, ProgramNode, ProgramItem, InstructionNode, LabelNode, PseudoSectionNode, PseudoGlobalNode, PseudoEquNode, PseudoFillNode, PseudoIntegerNode, PseudoStringNode, PseudoCommNode, ValueNode, OperandNode, RegisterNode, MemNode}, common::Size, instruction};

use self::scanner::{Scanner, Token};

mod scanner;

/// 对一个 `Option<Token>` 或 `Option<&Token>`进行模式匹配, 匹配特定的 `Some(Token)` 或 `Some(&Token)`, 在不匹配时 panic
///
/// 参数为:
/// - 进行匹配的 `Option<Token>` 或 `Option<&Token>`
/// - 一个实现了 `Display` 的类型, 用于匹配失败时 panic 的信息一部分(`"syntax error: expected {}, but found ..."`)
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
    lookahead: Option<Token>,  // 储存向前看的一个符号, 但是不要直接使用, 应用 lookahead(), next_token() 代替
}

impl<R: BufRead> Parser<R> {
    pub fn new(reader: R) -> Self {
        Self {
            scanner: Scanner::new(reader),
            lookahead: None,
        }
    }

    pub fn build_ast(&mut self) -> Ast {
        Ast::new(self.program())
    }

    fn lookahead(&mut self) -> Option<&Token> {
        if self.lookahead.is_none() {
            self.lookahead = self.scanner.next();
        }
        self.lookahead.as_ref()
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.lookahead {
            Some(_) => self.lookahead.take(),
            None => self.scanner.next(),
        }
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
        let (mnemonic, operand_size) = match_token! {
            self.next_token(), "mnemonic",
            Token::Mnemonic(mnemonic, operand_size) => (mnemonic, operand_size),
        };
        let mut operands = Vec::new();

        if instruction::is_jump(&mnemonic) {
            operands.push(self.jump_operand());
        } else if self.lookahead().is_some() && self.lookahead().unwrap() != &Token::Eol {
            operands.push(self.none_jump_operand()); // first operand
            while self.lookahead().is_some() && self.lookahead().unwrap() == &Token::Comma {
                self.next_token();
                operands.push(self.none_jump_operand());
            }
        }

        InstructionNode { mnemonic, operand_size, operands }
    }

    fn jump_operand(&mut self) -> OperandNode {
        match_token! {
            self.lookahead(), "jump operand",
            Token::Symbol(_) | Token::Integer(_) => OperandNode::Immediate(self.value()),
            Token::Star => {
                self.next_token();
                self.register_or_mem_operand()
            },
        }
    }

    fn none_jump_operand(&mut self) -> OperandNode {
        match_token! {
            self.lookahead(), "none-jump operand",
            Token::Dollar => {
                self.next_token();
                OperandNode::Immediate(self.value())
            },
            Token::Register(_) | Token::Symbol(_) | Token::Integer(_) | Token::Lparen => self.register_or_mem_operand(),
        }
    }

    fn register_or_mem_operand(&mut self) -> OperandNode {
        match_token! {
            self.lookahead(), "register or mem",
            Token::Register(_) => {
                let reg = self.register();
                match_token! {
                    self.lookahead(), "",
                    Token::Colon => {
                        self.next_token();
                        OperandNode::Memory(self.mem(), Some(reg))
                    },
                    _ => OperandNode::Register(reg),
                }
            },
            Token::Symbol(_) | Token::Integer(_) | Token::Lparen => OperandNode::Memory(self.mem(), None),
        }
    }

    fn register(&mut self) -> RegisterNode {
        match_token! {
            self.next_token(), "register",
            Token::Register(name) => RegisterNode { name }
        }
    }

    fn mem(&mut self) -> MemNode {
        let offset = match_token! {
            self.lookahead(), "value or lparen",
            Token::Symbol(_) | Token::Integer(_) => Some(self.value()),
            Token::Lparen => None,
        };

        match self.lookahead() {
            Some(Token::Lparen) => {
                self.next_token();
            },
            _ => return MemNode { offset, base: None, index_scale: None},
        }

        let base = match_token! {
            self.lookahead(), "register or comma",
            Token::Register(_) => Some(self.register()),
            Token::Comma => None,
        };

        match_token! {
            self.next_token(), "rparen or comma",
            Token::Rparen => return MemNode { offset, base, index_scale: None},
            Token::Comma => (),
        }

        let index = match_token! {
            self.lookahead(), "register",
            Token::Register(_) => self.register(),
        };

        match_token! {
            self.next_token(), "rparen or comma",
            Token::Rparen => return MemNode { offset, base, index_scale: Some((index, 1))},
            Token::Comma => (),
        }

        let scale = match_token! {
            self.next_token(), "integer",
            Token::Integer(value) => value,
        };

        match_token! {self.next_token(), "rparen", Token::Rparen }

        MemNode { offset, base, index_scale: Some((index, scale)) }
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

    #[test]
    fn test_instruction() {
        let code = r#"
        main:
            pusha
            pushf
            jmp *0x1234
            int $0x80
        hehe:
            pushb $0x10
            push %eax
            movl %ebx, 0x01(%eax, %ebx, 2)
        "#.trim();
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        let parser_ast = parser.build_ast();

        let mut printer = AstPrinter::new();
        parser_ast.run_visitor(&mut printer);
        println!("");
        println!("{}", printer.as_str());
    }

    #[test]
    #[should_panic]
    fn test_syntex_error_comma() {
        let code = r#"
        main:
            pusha
            pushf
            jmp *0x1234
            int $0x80
        hehe:
            pushb $0x10
            push %eax, # here should panic
            movl %ebx, 0x01(%eax, %ebx, 2)
        "#.trim();
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        parser.build_ast();
    }

    #[test]
    fn test_segment_register_mem_operand() {
        let code = "movb $2, %ds:(%eax, %ebx, 1)";
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        let ast = parser.build_ast();

        let mut printer = AstPrinter::new();
        ast.run_visitor(&mut printer);
        println!("");
        println!("{}", printer.as_str());
    }
}