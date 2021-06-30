pub mod ast;
pub mod compiler;
pub mod object;
pub mod opcode;
pub mod parser;
pub mod scanner;
pub mod vm;
use std::io::{self, BufRead};

fn main() {
    loop {
        let mut line = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut line).unwrap();

        let lexer = scanner::Scanner::new(&line);
        let mut parser = parser::Parser::new(lexer);

        let root_node = parser.parse_root_node();
        let root_node = ast::Node::Root(Box::new(root_node));

        let mut compiler = compiler::Compiler::new();
        let res = compiler.compile(root_node);
        assert!(!res.is_none());

        let mut vm = vm::VM::new(compiler.consts, compiler.insts);

        let res = vm.run();
        assert!(!res.is_none());

        let res = vm.get_last();

        match &res {
            object::Object::Integer(val) => println!("{}", val.value),
            object::Object::String(val) => println!("{}", val.value),
            object::Object::Float(val) => println!("{}", val.value),
            object::Object::Bool(val) => println!("{}", val.value),
            _ => println!("object display not supported"),
        };
    }
}
