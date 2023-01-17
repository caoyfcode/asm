use std::io::BufRead;

use crate::common::{Size, Error};
use crate::instruction;
use crate::ast::{Ast, ProgramNode, ProgramItem, InstructionNode, LabelNode, PseudoSectionNode, PseudoGlobalNode, PseudoEquNode, PseudoFillNode, PseudoIntegerNode, PseudoStringNode, PseudoCommNode, ValueNode, OperandNode, RegisterNode, MemNode};

use self::scanner::{Scanner, Token};

mod scanner;

/// 对一个 `Option<Token>` 或 `Option<&Token>`进行模式匹配, 匹配特定的 `Some(Token)` 或 `Some(&Token)`, 并返回一个 `Result<T, crate::common::Error>`
///
/// 参数为:
/// - 进行匹配的 `Option<Token>` 或 `Option<&Token>`
/// - 逗号分隔的模式列表, 若要返回值, 使用 `<pat> => <expr>` 形式, 不返回值则使用 `<pat>`, 要返回的值会包裹到 `Result` 中
macro_rules! match_token {
    ($t:expr, $($pat:pat => $out:expr),+ $(,)?) => {
        match $t {
            #[allow(unreachable_code)]
            $(Some($pat) => Ok($out),)+
            Some(Token::Err(err)) => Err(err.clone()),
            _ => Err($crate::common::Error::UnexpectedSymbol),
        }
    };
    ($t:expr, $($pat:pat),+ $(,)?) => {
        match $t {
            #[allow(unreachable_code)]
            $(Some($pat) => Ok(()),)+
            Some(Token::Err(err)) => Err(err.clone()),
            _ => Err($crate::common::Error::UnexpectedSymbol),
        }
    };
}

type Result<T> = core::result::Result<T, Error>;

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

    pub fn build_ast(&mut self) -> Result<Ast> {
        Ok(Ast::new(self.program()?))
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

    fn program(&mut self) -> Result<ProgramNode> {
        let mut items = Vec::new();

        while self.lookahead().is_some() {
            let mut should_match_eol = true;
            let line = self.scanner.last_token_info().0;
            match_token!(
                self.lookahead(),
                Token::DotSection => items.push(
                    (ProgramItem::PseudoSection(self.pseudo_section()?), line)
                ),
                Token::DotGlobal => items.push(
                    (ProgramItem::PseudoGlobal(self.pseudo_global()?), line)
                ),
                Token::DotEqu => items.push(
                    (ProgramItem::PseudoEqu(self.pseudo_equ()?), line)
                ),
                Token::DotFill => items.push(
                    (ProgramItem::PseudoFill(self.pseudo_fill()?), line)
                ),
                Token::DotByte | Token::DotWord | Token::DotLong => items.push(
                    (ProgramItem::PseudoInteger(self.pseudo_integer()?), line)
                ),
                Token::DotAscii | Token::DotAsciz=> items.push(
                    (ProgramItem::PseudoString(self.pseudo_string()?), line)
                ),
                Token::DotComm | Token::DotLcomm => items.push(
                    (ProgramItem::PseudoComm(self.pseudo_comm()?), line)
                ),
                Token::Mnemonic(_, _) => items.push(
                    (ProgramItem::Instruction(self.instruction()?), line)
                ),
                Token::Symbol(_) => {
                    items.push((ProgramItem::Label(self.label()?), line));
                    should_match_eol = false;
                },
                Token::Eol => (),
            )?;
            if should_match_eol {
                match_token!(self.next_token(), Token::Eol)?;
            }
        }

        Ok(ProgramNode { items })
    }

    fn pseudo_section(&mut self) -> Result<PseudoSectionNode> {
        match_token!(self.next_token(), Token::DotSection)?;
        match_token!(
            self.next_token(),
            Token::Symbol(symbol) => PseudoSectionNode { symbol },
        )
    }

    fn pseudo_global(&mut self) -> Result<PseudoGlobalNode> {
        match_token!(self.next_token(), Token::DotGlobal)?;
        match_token!(
            self.next_token(),
            Token::Symbol(symbol) => PseudoGlobalNode { symbol },
        )
    }

    fn pseudo_equ(&mut self) -> Result<PseudoEquNode> {
        match_token!(self.next_token(), Token::DotEqu)?;
        let symbol = match_token!(
            self.next_token(),
            Token::Symbol(symbol) => symbol,
        )?;
        match_token!(self.next_token(), Token::Comma)?;
        let value = match_token!(
            self.next_token(),
            Token::Integer(value) => value,
        )?;
        Ok(PseudoEquNode { symbol, value })
    }

    fn pseudo_fill(&mut self) -> Result<PseudoFillNode> {
        match_token!(self.next_token(), Token::DotFill)?;
        let repeat = match_token!(
            self.next_token(),
            Token::Integer(value) => value,
        )?;
        match_token!(self.next_token(), Token::Comma)?;
        let size = match_token!(
            self.next_token(),
            Token::Integer(value) => value,
        )?;
        match_token!(self.next_token(), Token::Comma)?;
        let value = self.value()?;

        Ok(PseudoFillNode { repeat, size, value })
    }

    fn value(&mut self) -> Result<ValueNode> {
        match_token!(
            self.next_token(),
            Token::Integer(value) => ValueNode::Integer(value),
            Token::Symbol(symbol) => ValueNode::Symbol(symbol),
        )
    }

    fn pseudo_integer(&mut self) -> Result<PseudoIntegerNode> {
        let size = match_token!(
            self.next_token(),
            Token::DotByte => Size::Byte,
            Token::DotWord => Size::Word,
            Token::DotLong => Size::DoubleWord,
        )?;
        let mut values = Vec::new();
        values.push(self.value()?);
        while self.lookahead().is_some() && self.lookahead().unwrap() == &Token::Comma {
            self.next_token();
            values.push(self.value()?);
        }
        Ok(PseudoIntegerNode { size, values })
    }

    fn pseudo_string(&mut self) -> Result<PseudoStringNode> {
        let zero_end = match_token!(
            self.next_token(),
            Token::DotAscii => false,
            Token::DotAsciz => true,
        )?;
        let content = match_token!(
            self.next_token(),
            Token::String(str) => str,
        )?;
        Ok(PseudoStringNode { zero_end, content })
    }

    fn pseudo_comm(&mut self) -> Result<PseudoCommNode> {
        let is_local = match_token!(
            self.next_token(),
            Token::DotLcomm => true,
            Token::DotComm => false,
        )?;
        let symbol = match_token!(
            self.next_token(),
            Token::Symbol(symbol) => symbol,
        )?;
        match_token!(self.next_token(), Token::Comma)?;
        let length = self.value()?;
        Ok(PseudoCommNode { is_local, symbol, length })
    }

    fn instruction(&mut self) -> Result<InstructionNode> {
        let (mnemonic, operand_size) = match_token!(
            self.next_token(),
            Token::Mnemonic(mnemonic, operand_size) => (mnemonic, operand_size),
        )?;
        let mut operands = Vec::new();

        if instruction::is_jump(&mnemonic) { // jump instruction
            operands.push(self.jump_operand()?);
        } else { // none-jump instruction
            let has_operand = match self.lookahead() {
                Some(Token::Dollar | Token::Register(_) | Token::Symbol(_) | Token::Integer(_) | Token::Lparen) => true,
                _ => false,
            };
            if has_operand {
                operands.push(self.none_jump_operand()?); // first operand
                while self.lookahead().is_some() && self.lookahead().unwrap() == &Token::Comma {
                    self.next_token();
                    operands.push(self.none_jump_operand()?);
                }
            }
        }

        Ok(InstructionNode { mnemonic, operand_size, operands })
    }

    fn jump_operand(&mut self) -> Result<OperandNode> {
        match_token!(
            self.lookahead(),
            Token::Symbol(_) | Token::Integer(_) => OperandNode::Immediate(self.value()?),
            Token::Star => {
                self.next_token();
                self.register_or_mem_operand()?
            },
        )
    }

    fn none_jump_operand(&mut self) -> Result<OperandNode> {
        match_token!(
            self.lookahead(),
            Token::Dollar => {
                self.next_token();
                OperandNode::Immediate(self.value()?)
            },
            Token::Register(_) | Token::Symbol(_) | Token::Integer(_) | Token::Lparen => self.register_or_mem_operand()?,
        )
    }

    fn register_or_mem_operand(&mut self) -> Result<OperandNode> {
        match_token!(
            self.lookahead(),
            Token::Register(_) => {
                let reg = self.register()?;
                match self.lookahead() {
                    Some(Token::Colon) => {
                        self.next_token();
                        OperandNode::Memory(self.mem()?, Some(reg))
                    },
                    _ => OperandNode::Register(reg),
                }
            },
            Token::Symbol(_) | Token::Integer(_) | Token::Lparen => OperandNode::Memory(self.mem()?, None),
        )
    }

    fn register(&mut self) -> Result<RegisterNode> {
        match_token!(
            self.next_token(),
            Token::Register(name) => RegisterNode { name }
        )
    }

    fn mem(&mut self) -> Result<MemNode> {
        let offset = match_token!(
            self.lookahead(),
            Token::Symbol(_) | Token::Integer(_) => Some(self.value()?),
            Token::Lparen => None,
        )?;

        match self.lookahead() {
            Some(Token::Lparen) => {
                self.next_token();
            },
            _ => return Ok(MemNode { offset, base: None, index_scale: None}),
        }

        let base = match_token!(
            self.lookahead(),
            Token::Register(_) => Some(self.register()?),
            Token::Comma => None,
        )?;

        match_token!(
            self.next_token(),
            Token::Rparen => return Ok(MemNode { offset, base, index_scale: None}),
            Token::Comma => (),
        )?;

        let index = match_token!(
            self.lookahead(),
            Token::Register(_) => self.register()?,
        )?;

        match_token!(
            self.next_token(),
            Token::Rparen => return Ok(MemNode { offset, base, index_scale: Some((index, 1))}),
            Token::Comma => (),
        )?;

        let scale = match_token!(
            self.next_token(),
            Token::Integer(value) => value,
        )?;

        match_token!(self.next_token(), Token::Rparen)?;

        Ok(MemNode { offset, base, index_scale: Some((index, scale)) })
    }

    fn label(&mut self) -> Result<LabelNode> {
        let label = match_token!(
            self.next_token(),
            Token::Symbol(symbol) => symbol,
        )?;
        match_token!(self.next_token(), Token::Colon)?;
        Ok(LabelNode { label })
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
        let parser_ast = parser.build_ast().unwrap();

        let mut printer = AstPrinter::new();
        parser_ast.run_visitor(&mut printer);
        println!("");
        println!("{}", printer.as_str());
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
        let parser_ast = parser.build_ast().unwrap();

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
        parser.build_ast().unwrap();
    }

    #[test]
    fn test_segment_register_mem_operand() {
        let code = "movb $2, %ds:(%eax, %ebx, 1)";
        let cursor = Cursor::new(code);
        let mut parser = Parser::new(cursor);
        let ast = parser.build_ast().unwrap();

        let mut printer = AstPrinter::new();
        ast.run_visitor(&mut printer);
        println!("");
        println!("{}", printer.as_str());
    }
}