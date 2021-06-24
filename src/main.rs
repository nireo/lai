pub mod ast;
pub mod parser;
pub mod scanner;
use std::io::{self, BufRead};

fn main() {
    let mut line = String::new();
    let stdin = io::stdin();
    stdin.lock().read_line(&mut line).unwrap();
    println!("{}", line);

    let mut lexer = scanner::Scanner::new(&line);

    let mut idx = 0;
    loop {
        idx += 1;

        print!("{}: ", idx);
        let token = lexer.next_token();
        if token == scanner::Token::EOF {
            break;
        }
    }
}
