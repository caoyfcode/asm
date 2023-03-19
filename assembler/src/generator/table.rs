use std::collections::{HashMap, HashSet};

use elf::{Symbol, ProgramSection};

use super::Section;

#[derive(Clone, Copy)]
pub(super) enum SymbolKind {
    External, // 外部符号
    Label(u32, Section), // 文件中定义的标签
    Constant(u32), // .equ/.set 定义的静态符号
}

pub(super) struct SymbolTable {
    globals: HashSet<String>, // 标注哪些符号是 global 的, 与 .global 对应
    symbols: HashMap<String, SymbolKind>, // 标签
}

impl SymbolTable {
    pub(super) fn new() -> Self {
        Self {
            globals: HashSet::new(),
            symbols: HashMap::new(),
        }
    }

    pub(super) fn get_symbol(&self, name: &String) -> Option<SymbolKind> {
        self.symbols.get(name).copied()
    }

    pub(super) fn symbols(&self) -> Vec<Symbol> {
        let mut symbols: Vec<Symbol> = Vec::new();
        for (name, kind) in &self.symbols {
            match kind {
                SymbolKind::External => symbols.push(Symbol {
                    name: name.clone(),
                    value: 0,
                    section: ProgramSection::Undef,
                    is_global: true,
                }),
                SymbolKind::Label(offset, section) => symbols.push(Symbol {
                    name: name.clone(),
                    value: *offset,
                    section: (*section).into(),
                    is_global: self.globals.contains(name),
                }),
                SymbolKind::Constant(_) => (),
            }
        }
        symbols.sort_by(|lhs, rhs| {
            let ord = (lhs.section as usize).cmp(&(rhs.section as usize));
            if ord.is_eq() {
                let ord = lhs.value.cmp(&rhs.value);
                if ord.is_eq() {
                    lhs.name.cmp(&rhs.name)
                } else {
                    ord
                }
            } else {
                ord
            }
        });
        symbols
    }

    pub(super) fn insert_constant(&mut self, name: String, value: u32) -> bool {
        if self.globals.contains(&name) {
            return false;
        }
        if let Some(SymbolKind::External | SymbolKind::Label(..)) = self.symbols.get(&name) {
            return false;
        }
        self.symbols.insert(name, SymbolKind::Constant(value));
        true
    }

    pub(super) fn insert_external(&mut self, name: String) -> bool {
        if let Some(SymbolKind::Label(..) | SymbolKind::Constant(_)) = self.symbols.get(&name) {
            return false;
        }
        self.symbols.insert(name, SymbolKind::External);
        true
    }

    pub(super) fn insert_label(&mut self, name: String, offset:u32, section: Section) -> bool {
        if let Some(SymbolKind::Label(..) | SymbolKind::Constant(_)) = self.symbols.get(&name) {
            return false;
        }
        self.symbols.insert(name, SymbolKind::Label(offset, section));
        true
    }

    pub(super) fn set_global(&mut self, name: String) -> bool {
        if let Some(SymbolKind::Constant(_)) = self.symbols.get(&name) {
            return false;
        }
        self.globals.insert(name);
        true
    }
}