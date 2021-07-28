pub mod ast;
pub mod builtin;
pub mod compiler;
pub mod object;
pub mod opcode;
pub mod parser;
pub mod regvm;
pub mod scanner;
pub mod vm;

use std::env;
use std::io::prelude::*;
use std::io::{self, BufRead};

fn parse_program(input: &str) -> ast::Node {
    let lexer = scanner::Scanner::new(input);
    let mut parser = parser::Parser::new(lexer);

    let root_node = parser.parse_root_node();
    assert!(root_node.is_some());

    ast::Node::Root(Box::new(root_node.unwrap()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args[1] == "repl" {
        loop {
            let mut line = String::new();
            let stdin = io::stdin();
            stdin.lock().read_line(&mut line).unwrap();

            let root_node = parse_program(&line);

            let mut compiler = compiler::Compiler::new();
            let res = compiler.compile(root_node);
            if res.is_err() {
                println!("{}", res.as_ref().err().unwrap())
            }
            assert!(!res.is_err());

            let instructions = compiler.get_insts().clone();
            let mut vm = vm::VM::new(compiler.consts, instructions);

            let res = vm.run();
            if res.is_err() {
                println!("{}", res.as_ref().err().unwrap())
            }
            assert!(!res.is_err());

            let top = vm.get_last();
            match &top {
                object::Object::Integer(val) => println!("{}", val),
                object::Object::String(val) => println!("{}", val),
                object::Object::Float(val) => println!("{}", val),
                object::Object::Bool(val) => println!("{}", val),
                _ => println!("object display not supported"),
            };
        }
    } else {
        let mut file = std::fs::File::open(&args[1]).expect("Unable to open the file");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Unable to read the file");

        let root_node = parse_program(&contents);

        let mut compiler = compiler::Compiler::new();
        let res = compiler.compile(root_node);
        if res.is_err() {
            println!("{}", res.as_ref().err().unwrap())
        }
        assert!(!res.is_err());

        let instructions = compiler.get_insts().clone();
        let mut vm = vm::VM::new(compiler.consts, instructions);

        let res = vm.run();
        if res.is_err() {
            println!("{}", res.as_ref().err().unwrap())
        }
        assert!(!res.is_err());
    }
}
