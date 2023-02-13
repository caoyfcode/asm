use std::collections::{HashMap, HashSet};

use super::Section;

#[derive(Clone, Copy)]
pub(super) enum SymbolKind {
    External, // 外部符号
    Label(u32, Section), // 文件中定义的标签
    Equ(u32), // .equ/.set 定义的静态符号
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

    pub(super) fn labels_and_externals(&self) -> (Vec<(String, u32, Section, bool)>, Vec<String>) {
        let mut labels: Vec<(String, u32, Section, bool)> = Vec::new(); // name, offset, section, is_global
        let mut externals: Vec<String> = Vec::new();
        for (name, kind) in &self.symbols {
            match kind {
                SymbolKind::External => externals.push(name.clone()),
                SymbolKind::Label(offset, section) => labels.push((name.clone(), *offset, *section, self.globals.contains(name))),
                SymbolKind::Equ(_) => (),
            }
        }
        labels.sort_by(|a, b| {
            let ord = a.2.name().cmp(b.2.name());
            if ord.is_eq() {
                return a.1.cmp(&b.1);
            }
            ord
        });
        externals.sort();
        (labels, externals)
    }

    pub(super) fn insert_equ(&mut self, name: String, value: u32) -> bool {
        if self.globals.contains(&name) {
            return false;
        }
        if let Some(SymbolKind::External | SymbolKind::Label(..)) = self.symbols.get(&name) {
            return false;
        }
        self.symbols.insert(name, SymbolKind::Equ(value));
        true
    }

    pub(super) fn insert_external(&mut self, name: String) -> bool {
        if let Some(SymbolKind::Label(..) | SymbolKind::Equ(_)) = self.symbols.get(&name) {
            return false;
        }
        self.symbols.insert(name, SymbolKind::External);
        true
    }

    pub(super) fn insert_label(&mut self, name: String, offset:u32, section: Section) -> bool {
        if let Some(SymbolKind::Label(..) | SymbolKind::Equ(_)) = self.symbols.get(&name) {
            return false;
        }
        self.symbols.insert(name, SymbolKind::Label(offset, section));
        true
    }

    pub(super) fn set_global(&mut self, name: String) -> bool {
        if let Some(SymbolKind::Equ(_)) = self.symbols.get(&name) {
            return false;
        }
        self.globals.insert(name);
        true
    }
}