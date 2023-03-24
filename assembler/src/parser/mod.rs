use std::io::BufRead;

use crate::common::{Size, Error};
use crate::config::{info_of_register, info_of_mnemonic};
use crate::ast::{Ast, ProgramNode, ProgramItem, InstructionNode, LabelNode, PseudoSectionNode, PseudoGlobalNode, PseudoEquNode, PseudoFillNode, PseudoIntegerNode, PseudoStringNode, PseudoCommNode, ValueNode, OperandNode, RegisterNode, MemNode};

use self::scanner::{Scanner, TokenKind, Token};

mod scanner;

/// 对一个 `Token` 或 `&Token` 进行模式匹配, 匹配特定的 `TokenKind`, 并返回一个 `Result<T, crate::common::Error>` (`Err` 代表匹配失败)
///
/// 参数为:
/// - 被匹配的 `Token` 或 `&Token`
/// - 期望的符号的描述, 用于匹配失败时的错误信息
/// - 逗号分隔的模式列表, `<pat>` 为 `TokenKind` 的模式, 若要返回值, 使用 `<pat> => <expr>` 形式, 不返回值则使用 `<pat>`, 要返回的值会包裹到 `Result` 中
macro_rules! match_token {
    ($t:expr, $expected:expr, $($pat:pat => $out:expr),+ $(,)?) => {
        match $t {
            #[allow(unreachable_code)]
            $( Token { kind: $pat, .. } => Ok($out), )+
            Token { kind: TokenKind::Err(err), .. } => Err(err.clone()),
            Token { line, content, ..} => Err($crate::common::Error::UnexpectedSymbol(line.clone(), String::from($expected), String::from(content))),
        }
    };
    ($t:expr, $expected:expr, $($pat:pat),+ $(,)?) => {
        match $t {
            #[allow(unreachable_code)]
            $( Token { kind: $pat, .. } => Ok(()), )+
            Token { kind: TokenKind::Err(err), .. } => Err(err.clone()),
            Token { line, content, ..} => Err($crate::common::Error::UnexpectedSymbol(line.clone(), String::from($expected), String::from(content))),
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

    // 返回符号, 行号, 符号内容
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
            let line = self.lookahead().unwrap().line;
            match_token!(
                self.lookahead().unwrap(), "pseudo or instruction or label",
                TokenKind::Label(label) => { // label
                    items.push((ProgramItem::Label(LabelNode { label: label.clone() }), line));
                    self.next_token();
                    should_match_eol = false;
                },
                TokenKind::Symbol(name) => { // pseudo or instruction
                    match name.as_str() {
                        ".section" => items.push(
                            (ProgramItem::PseudoSection(self.pseudo_section()?), line)
                        ),
                        ".global" | ".globl" => items.push(
                            (ProgramItem::PseudoGlobal(self.pseudo_global()?), line)
                        ),
                        ".equ" | ".set" => items.push(
                            (ProgramItem::PseudoEqu(self.pseudo_equ()?), line)
                        ),
                        ".fill" => items.push(
                            (ProgramItem::PseudoFill(self.pseudo_fill()?), line)
                        ),
                        ".byte" => items.push(
                            (ProgramItem::PseudoInteger(self.pseudo_integer(Size::Byte)?), line)
                        ),
                        ".word" | ".short" => items.push(
                            (ProgramItem::PseudoInteger(self.pseudo_integer(Size::Word)?), line)
                        ),
                        ".long" | ".int" => items.push(
                            (ProgramItem::PseudoInteger(self.pseudo_integer(Size::DoubleWord)?), line)
                        ),
                        ".ascii" => items.push(
                            (ProgramItem::PseudoString(self.pseudo_string(false)?), line)
                        ),
                        ".asciz" | ".string" => items.push(
                            (ProgramItem::PseudoString(self.pseudo_string(true)?), line)
                        ),
                        ".lcomm" => items.push(
                            (ProgramItem::PseudoComm(self.pseudo_lcomm()?), line)
                        ),
                        _ => items.push(
                            (ProgramItem::Instruction(self.instruction()?), line)
                        ),
                    }
                },
                TokenKind::Eol => (),
            )?;
            if should_match_eol {
                match_token!(
                    self.next_token().unwrap(), "Eol",
                    TokenKind::Eol,
                )?;
            }
        }

        Ok(ProgramNode { items })
    }

    fn pseudo_section(&mut self) -> Result<PseudoSectionNode> {
        self.next_token();
        match_token!(
            self.next_token().unwrap(), "symbol",
            TokenKind::Symbol(symbol) => PseudoSectionNode { symbol },
        )
    }

    fn pseudo_global(&mut self) -> Result<PseudoGlobalNode> {
        self.next_token();
        match_token!(
            self.next_token().unwrap(), "symbol",
            TokenKind::Symbol(symbol) => PseudoGlobalNode { symbol },
        )
    }

    fn pseudo_equ(&mut self) -> Result<PseudoEquNode> {
        self.next_token();
        let symbol = match_token!(
            self.next_token().unwrap(), "symbol",
            TokenKind::Symbol(symbol) => symbol,
        )?;
        match_token!(self.next_token().unwrap(), "\",\"", TokenKind::Comma)?;
        let value = match_token!(
            self.next_token().unwrap(), "integer",
            TokenKind::Integer(value) => value,
        )?;
        Ok(PseudoEquNode { symbol, value })
    }

    fn pseudo_fill(&mut self) -> Result<PseudoFillNode> {
        self.next_token();
        let repeat = match_token!(
            self.next_token().unwrap(), "integer",
            TokenKind::Integer(value) => value,
        )?;
        match_token!(self.next_token().unwrap(), "\",\"", TokenKind::Comma)?;
        let size = match_token!(
            self.next_token().unwrap(), "integer",
            TokenKind::Integer(value) => value,
        )?;
        match_token!(self.next_token().unwrap(), "\",\"", TokenKind::Comma)?;
        let value = self.value()?;

        Ok(PseudoFillNode { repeat, size, value })
    }

    fn value(&mut self) -> Result<ValueNode> {
        match_token!(
            self.next_token().unwrap(), "integer or symbol",
            TokenKind::Integer(value) => ValueNode::Integer(value),
            TokenKind::Symbol(symbol) => ValueNode::Symbol(symbol),
        )
    }

    fn pseudo_integer(&mut self, size: Size) -> Result<PseudoIntegerNode> {
        self.next_token();
        let mut values = Vec::new();
        values.push(self.value()?);
        while self.lookahead().unwrap().kind == TokenKind::Comma {
            self.next_token();
            values.push(self.value()?);
        }
        Ok(PseudoIntegerNode { size, values })
    }

    fn pseudo_string(&mut self, zero_end: bool) -> Result<PseudoStringNode> {
        self.next_token();
        let content = match_token!(
            self.next_token().unwrap(), "string",
            TokenKind::String(str) => str,
        )?;
        Ok(PseudoStringNode { zero_end, content })
    }

    fn pseudo_lcomm(&mut self) -> Result<PseudoCommNode> {
        self.next_token();
        let symbol = match_token!(
            self.next_token().unwrap(), "symbol",
            TokenKind::Symbol(symbol) => symbol,
        )?;
        match_token!(self.next_token().unwrap(), "\",\"", TokenKind::Comma)?;
        let length = self.value()?;
        Ok(PseudoCommNode { is_local: true, symbol, length })
    }

    fn instruction(&mut self) -> Result<InstructionNode> {
       let (mnemonic, line) =  match self.next_token().unwrap() {
            Token { kind, line, ..} => {
                match kind {
                    TokenKind::Symbol(mnemonic) => (mnemonic, line),
                    _ => panic!("can't be here"), // 调用该函数前已经确定是 Symbol 了
                }
            }
        };

        let (mnemonic, operand_size, is_jump) = match info_of_mnemonic(&mnemonic) {
            Some(info) => info,
            None => return Err(Error::UnknownSymbol(line, mnemonic)),
        };

        let mut operands = Vec::new();

        if is_jump { // jump instruction
            operands.push(self.jump_operand()?);
        } else if self.lookahead().unwrap().kind != TokenKind::Eol { // none-jump instruction has operand
            operands.push(self.none_jump_operand()?); // first operand
            while self.lookahead().unwrap().kind == TokenKind::Comma {
                self.next_token();
                operands.push(self.none_jump_operand()?);
            }
        }

        Ok(InstructionNode { mnemonic: String::from(mnemonic), operand_size, operands })
    }

    fn jump_operand(&mut self) -> Result<OperandNode> {
        match_token!(
            self.lookahead().unwrap(), "jump operand",
            TokenKind::Symbol(_) | TokenKind::Integer(_) => OperandNode::Immediate(self.value()?),
            TokenKind::Star => {
                self.next_token();
                self.register_or_mem_operand()?
            },
        )
    }

    fn none_jump_operand(&mut self) -> Result<OperandNode> {
        match_token!(
            self.lookahead().unwrap(), "operand",
            TokenKind::Dollar => {
                self.next_token();
                OperandNode::Immediate(self.value()?)
            },
            TokenKind::Register(_) | TokenKind::Symbol(_) | TokenKind::Integer(_) | TokenKind::Lparen => self.register_or_mem_operand()?,
        )
    }

    fn register_or_mem_operand(&mut self) -> Result<OperandNode> {
        match_token!(
            self.lookahead().unwrap(), "register or mem operand",
            TokenKind::Register(_) => {
                let reg = self.register()?;
                match self.lookahead().unwrap().kind {
                    TokenKind::Colon => {
                        self.next_token();
                        OperandNode::Memory(self.mem()?, Some(reg))
                    },
                    _ => OperandNode::Register(reg),
                }
            },
            TokenKind::Symbol(_) | TokenKind::Integer(_) | TokenKind::Lparen => OperandNode::Memory(self.mem()?, None),
        )
    }

    fn register(&mut self) -> Result<RegisterNode> {
        let line = self.lookahead().unwrap().line;
        let name = match_token! {
            self.next_token().unwrap(), "register",
            TokenKind::Register(name) => name,
        }?;
        if info_of_register(&name).is_some() {
            Ok(RegisterNode { name })
        } else {
            Err(Error::UnknownSymbol(line, name))
        }
    }

    fn mem(&mut self) -> Result<MemNode> {
        let displacement = match_token!(
            self.lookahead().unwrap(), "mem operand",
            TokenKind::Symbol(_) | TokenKind::Integer(_) => Some(self.value()?),
            TokenKind::Lparen => None,
        )?;

        match self.lookahead().unwrap().kind {
            TokenKind::Lparen => {
                self.next_token();
            },
            _ => return Ok(MemNode { displacement, base: None, index_scale: None}),
        }

        let base = match_token!(
            self.lookahead().unwrap(), "base register",
            TokenKind::Register(_) => Some(self.register()?),
            TokenKind::Comma => None,
        )?;

        match_token!(
            self.next_token().unwrap(), "\")\" or \",\"",
            TokenKind::Rparen => return Ok(MemNode { displacement, base, index_scale: None}),
            TokenKind::Comma => (),
        )?;

        let index = match_token!(
            self.lookahead().unwrap(), "index register",
            TokenKind::Register(_) => self.register()?,
        )?;

        match_token!(
            self.next_token().unwrap(), "\")\" or \",\"",
            TokenKind::Rparen => return Ok(MemNode { displacement, base, index_scale: Some((index, 1))}),
            TokenKind::Comma => (),
        )?;

        let scale = match_token!(
            self.next_token().unwrap(), "scale value",
            TokenKind::Integer(value) => value,
        )?;

        match_token!(self.next_token().unwrap(), "\")\"", TokenKind::Rparen)?;

        Ok(MemNode { displacement, base, index_scale: Some((index, scale)) })
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
    #[should_panic="8: error: expected operand, but found \"\""]
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
        match parser.build_ast() {
            Err(error) => panic!("{}", error),
            _ => (),
        }
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