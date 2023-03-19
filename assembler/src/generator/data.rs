use elf::Relocation;

use crate::common::Size;
use super::{Value, Statement};

pub(super) struct Data {
    size: Size,
    datas: Vec<Value>,
}

impl Data {
    pub(super) fn new(size: Size, datas: Vec<Value>) -> Self {
        Self {
            size,
            datas,
        }
    }

    pub(super) fn new_fill(repeat: u32, size: u32, value: u32) -> Self {
        let len = repeat as usize * size as usize;

        let mut datas: Vec<Value> = Vec::with_capacity(len);
        let size = if size > 4 { 4usize } else { size as usize };
        let value = [
            value & 0xff,
            (value >> 8) & 0xff,
            (value >> 16) & 0xff,
            (value >> 24) & 0xff,
        ];

        for _ in 0..repeat {
            for i in 0..size {
                datas.push(Value::Integer(value[i]));
            }
        }
        Self {
            size: Size::Byte,
            datas,
        }
    }

}

impl Statement for Data {
    fn length(&self) -> u32 {
        self.size.length() * self.datas.len() as u32
    }

    fn emit(&self) -> (Vec<u8>, Vec<Relocation>) {
        let mut data: Vec<u8> = Vec::with_capacity(self.length() as usize);
        let mut rel: Vec<Relocation> = Vec::new();
        let value_size = self.size.length();
        for value in &self.datas {
            match value {
                Value::Integer(val) => {
                    for i in 0..value_size {
                        data.push((*val >> (i * 8)) as u8);
                    }
                }
                Value::Symbol(name, is_relative) => { // addend 必然是 0
                    let offset = data.len() as u32;
                    for _ in 0..value_size {
                        data.push(0);
                    }
                    rel.push(Relocation { offset, symbol: name.clone(), is_relative: *is_relative });
                }
            }
        }
        (data, rel)
    }
}