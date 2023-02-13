/// .section 声明的节
#[derive(PartialEq, Clone, Copy)]
pub(super) enum Section {
    Text,
    Data,
    Bss,
}

impl Section {
    pub(super) fn from(name: &str) -> Option<Self> {
        match name {
            ".text" => Some(Self::Text),
            ".data" => Some(Self::Data),
            ".bss" => Some(Self::Bss),
            _ => None,
        }
    }

    pub(super) fn name(&self) -> &'static str {
        match self {
            Section::Text => ".text",
            Section::Data => ".data",
            Section::Bss => ".bss",
        }
    }
}

/// 用于在数据定义与指令中表示数值
pub(super) enum Value {
    Integer(u32), // 可以确定的数值
    Symbol(String, u32, bool), // 标签或外部符号, (name, addend, is_relative)
}

pub(super) struct RelocationInfo {
    pub(super) offset: u32, // 重定位地址
    pub(super) name: String, // 重定位符号名
    pub(super) is_relative: bool, // 是则 R_386_PC32, 否则 R_386_32
}

pub(super) trait Statement {
    fn length(&self) -> u32;
    fn emit(&self) -> (Vec<u8>, Vec<RelocationInfo>);
}