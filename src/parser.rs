pub use crate::scanner;

pub struct Parser {
		pub lexer: scanner::Scanner,
}

impl Parser {
		pub fn new(existing_lexer: scanner::Scanner) -> Self {
				Self {
						lexer: existing_lexer,
				}
		}
}
