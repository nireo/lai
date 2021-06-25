pub mod ast;
pub mod parser;
pub mod scanner;
use std::io::{self, BufRead};

fn main() {
    loop {
        let mut line = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut line).unwrap();

        let lexer = scanner::Scanner::new(&line);
        let mut parser = parser::Parser::new(lexer);

        let root_node = parser.parse_root_node();
        if root_node.statements.len() == 0 {
            println!("parsing unsuccessful");
        } else {
            println!("parsing successful");
        }
    }
}
