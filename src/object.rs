use crate::{ast::{self, FunctionParamNode}, opcode::{self, Inst}};

// This file contains definitions for different objects used in running the code
// through a virtual machine.

// use a generic definition for objects that only need to hold a single value.
#[derive(Clone, Debug)]
pub struct FunctionObject {
    pub params: Vec<FunctionParamNode>,
    pub body: ast::BlockNode,
}

#[derive(Clone, Debug)]
pub struct CompiledFunction {
    pub instructions: opcode::Inst,
}

impl CompiledFunction {
    pub fn new(instructions: Inst) -> Self {
        Self {
            instructions,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Object {
    Integer(i32),
    String(String),
    Float(f32),
    Bool(bool),
    Return(Box<Object>),
    Function(FunctionObject),
    Array(Vec<Object>),
    CompiledFunction(CompiledFunction),
    Error(String),
    Null,
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}
