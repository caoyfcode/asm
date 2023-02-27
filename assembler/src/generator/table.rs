use std::collections::{HashMap, HashSet};

use super::Section;

/// 表示一个标签
pub(super) struct Label {
    pub(super) name: String,
    pub(super) offset: u32,
    pub(super) section: Section,
    pub(super) is_global: bool,
}

impl Label {
    fn new(name: String, offset: u32, section: Section, is_global: bool) -> Self {
        Self { name, offset, section, is_global }
    }
}

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

    pub(super) fn labels_and_externals(&self) -> (Vec<Label>, Vec<String>) {
        let mut labels: Vec<Label> = Vec::new(); // name, offset, section, is_global
        let mut externals: Vec<String> = Vec::new();
        for (name, kind) in &self.symbols {
            match kind {
                SymbolKind::External => externals.push(name.clone()),
                SymbolKind::Label(offset, section) => labels.push(Label::new(name.clone(), *offset, *section, self.globals.contains(name))),
                SymbolKind::Constant(_) => (),
            }
        }
        labels.sort_by(|a, b| {
            let ord = a.section.name().cmp(b.section.name());
            if ord.is_eq() {
                return a.offset.cmp(&b.offset);
            }
            ord
        });
        externals.sort();
        (labels, externals)
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