use crate::{object, opcode::Inst};

pub struct Compiler {
    pub insts: Inst,
    pub consts: Vec<object::Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
            insts: Inst(Vec::new()),
        }
    }

    pub fn get_consts(&self) -> &[object::Object] {
        &self.consts
    }

    pub fn get_insts(&self) -> &Inst {
        &self.insts
    }
}
