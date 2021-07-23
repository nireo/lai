use crate::object::{self, Object};

fn builtin_len(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Null
    }

    match &args[0] {
        Object::Array(val) => Object::Integer(val.len() as i32),
        Object::String(val) => Object::Integer(val.len() as i32),
        _ => Object::Null
    }
}

fn builtin_from_name(name: &str) -> Option<fn(&[Object]) -> Object> {
    match name {
        "len" => Some(builtin_len),
        _ => None
    }
}
