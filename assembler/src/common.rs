/// 内存操作数的大小
#[derive(Debug, PartialEq, Clone)]
pub enum Size {
    Byte, Word, DoubleWord,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    // 词法错误
    ParseIntFail,
    UnknownSymbol,
    // 语法错误
    UnexpectedSymbol,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParseIntFail => "syntax error: number too large to fit u32",
            Error::UnknownSymbol => "syntax error: unknown symbol",
            Error::UnexpectedSymbol => "syntax error: unexpected symbol",
        }.fmt(f)
    }
}

impl std::error::Error for Error {}