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
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    // 词法错误
    ParseIntFail(usize, String), // line number and literal
    UnknownSymbol(usize, String), // line number and symbol
    // 语法错误
    UnexpectedSymbol(usize, String, String), // line number, expected description, found symbol
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
        }
    }
}

impl std::error::Error for Error {}