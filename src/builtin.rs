use crate::object::Object;

pub fn builtin_len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Null
    }

    match &args[0] {
        Object::Array(val) => Object::Integer(val.len() as i32),
        Object::String(val) => Object::Integer(val.len() as i32),
        _ => Object::Null
    }
}

pub fn builtin_print(args: Vec<Object>) -> Object {

    for obj in args.iter() {
        println!("{}", obj);
    }

    Object::Null
}

pub fn builtin_from_name(name: &str) -> Option<fn(Vec<Object>) -> Object> {
    match name {
        "len" => Some(builtin_len),
        "print" => Some(builtin_print),
        _ => None
    }
}

pub fn builtin_from_index(index: usize) -> Option<fn(Vec<Object>) -> Object> {
    match index {
        0 => Some(builtin_len),
        1 => Some(builtin_print),
        _ => None
    }
}
