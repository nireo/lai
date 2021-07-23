pub mod ast;
pub mod compiler;
pub mod object;
pub mod opcode;
pub mod parser;
pub mod scanner;
pub mod regvm;
pub mod vm;
pub mod builtin;
use std::io::{self, BufRead};

fn main() {
    loop {
        let mut line = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut line).unwrap();

        let lexer = scanner::Scanner::new(&line);
        let mut parser = parser::Parser::new(lexer);

        let root_node = parser.parse_root_node().unwrap();
        let root_node = ast::Node::Root(Box::new(root_node));

        let mut compiler = compiler::Compiler::new();
        let res = compiler.compile(root_node);
        assert!(!res.is_err());

        let mut vm = vm::VM::new(compiler.consts, compiler.insts);

        let res = vm.run();
        assert!(!res.is_err());

        let res = vm.get_last();

        match &res {
            object::Object::Integer(val) => println!("{}", val),
            object::Object::String(val) => println!("{}", val),
            object::Object::Float(val) => println!("{}", val),
            object::Object::Bool(val) => println!("{}", val),
            _ => println!("object display not supported"),
        };
    }
}
