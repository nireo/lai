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
    pub num_locals: usize,
    pub num_params: usize,
}

impl CompiledFunction {
    pub fn new(instructions: Inst) -> Self {
        Self {
            instructions,
            num_locals: 0,
            num_params: 0,
        }
    }

    pub fn new_with_params(instructions: Inst, num_locals: usize, num_params: usize) -> Self {
        Self {
            instructions,
            num_locals,
            num_params,
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
    Builtin(fn(Vec<Object>) -> Object),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::String(val) => write!(f, "{}", val),
            Object::Null => write!(f, "null"),
            Object::Float(val) => write!(f, "{}", val),
            Object::Bool(val) => write!(f, "{}", val),
            _ => write!(f, "non-printable-object"),
        }
    }
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}
