use std::fmt;

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

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Integer(_) => write!(f, "integer"),
            Object::String(_) => write!(f, "string"),
            Object::Null => write!(f, "null"),
            Object::Float(_) => write!(f, "float"),
            Object::Bool(_) => write!(f, "bool"),
            Object::Return(_) => write!(f, "return"),
            Object::Function(_) => write!(f, "function"),
            Object::Array(_) => write!(f, "array"),
            Object::CompiledFunction(_) => write!(f, "function"),
            _ => write!(f, "obj"),
        }
    }
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}
