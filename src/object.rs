use crate::ast::{self, FunctionParamNode};

// This file contains definitions for different objects used in running the code
// through a virtual machine.

// use a generic definition for objects that only need to hold a single value.
pub struct ValueObj<T> {
    pub value: T,
}

pub struct FunctionObject {
    pub params: Vec<FunctionParamNode>,
    pub body: ast::BlockNode,
}

pub enum Object {
    Integer(ValueObj<i32>),
    String(ValueObj<String>),
    Float(ValueObj<f32>),
    Bool(ValueObj<bool>),
    Return(ValueObj<Box<Object>>),
    Function(FunctionObject),

    // Error holds information about an error in an string, so we can just use a ValueObj
    Error(ValueObj<String>),

    Null,
}
