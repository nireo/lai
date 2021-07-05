use crate::ast::{self, FunctionParamNode};

// This file contains definitions for different objects used in running the code
// through a virtual machine.

// use a generic definition for objects that only need to hold a single value.
#[derive(Clone)]
pub struct FunctionObject {
    pub params: Vec<FunctionParamNode>,
    pub body: ast::BlockNode,
}

#[derive(Clone)]
pub enum Object {
    Integer(i32),
    String(String),
    Float(f32),
    Bool(bool),
    Return(Box<Object>),
    Function(FunctionObject),
    Array(Vec<Object>),

    // Error holds information about an error in an string, so we can just use a ValueObj
    Error(String),

    Null,
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}
