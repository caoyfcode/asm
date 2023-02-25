/// 内存操作数的大小
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Size {
    Byte, Word, DoubleWord,
}

impl Size {
    pub fn length(&self) -> u32 {
        match self {
            Size::Byte => 1,
            Size::Word => 2,
            Size::DoubleWord => 4,
        }
    }

    pub fn size_of(val: u32) -> Self {
        if val <= u8::MAX as u32 {
            Self::Byte
        } else if val <= u16::MAX as u32 {
            Self::Word
        } else {
            Self::DoubleWord
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    // 词法错误
    ParseIntFail(usize, String), // line number and literal
    UnknownSymbol(usize, String), // line number and symbol
    // 语法错误
    UnexpectedSymbol(usize, String, String), // line number, expected description, found symbol
    // 语义错误
    UnsupportedSection(usize, String), // line number, section name
    DefineSymbolFail(usize, String, String), // line number, name, def type
    UseWrongTypeSymbol(usize, String, String), // line number, name, need type
    NotInRightSection(usize, String), // line number, right section name
    InvalidOperands(usize, String), // line number, mnemonic
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParseIntFail(line, number) => {
                write!(f, "{line}: error: \"{number}\" too large to fit u32")
            },
            Error::UnknownSymbol(line, symbol) => {
                write!(f, "{line}: error: unknown symbol \"{symbol}\"")
            },
            Error::UnexpectedSymbol(line, expected, found) => {
                write!(f, "{line}: error: expected {expected}, but found \"{found}\"")
            },
            Error::UnsupportedSection(line, name) => {
                write!(f, "{line}: error: unsupported section \"{name}\"")
            },
            Error::DefineSymbolFail(line, name, kind) => {
                write!(f, "{line}: error: \"{name}\" can't be a new {kind}")
            },
            Error::UseWrongTypeSymbol(line, name, kind) => {
                write!(f, "{line}: error: \"{name}\" is not a {kind}")
            },
            Error::NotInRightSection(line, name) => {
                write!(f, "{line}: error: not in {name} section")
            },
            Error::InvalidOperands(line, mnemonic) => {
                write!(f, "{line}: error: operands of \"{mnemonic}\" are invalid")
            }
        }
    }
}

impl std::error::Error for Error {}