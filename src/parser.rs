use crate::ast;
pub use crate::scanner::*;

pub struct Parser {
		pub lexer: Scanner,

		current_token: Token,
		peek_token: Token,
}

impl Parser {
		pub fn new(existing_lexer: Scanner) -> Self {
				let mut parser = Self {
						lexer: existing_lexer,

						// use to random tokens, since rust needs all of the struct fields initialized.
						current_token: Token::EOF,
						peek_token: Token::EOF,
				};

				parser.next_token();
				parser.next_token();

				parser
		}

		fn next_token(&mut self) {
				self.current_token = self.peek_token.clone();
				self.peek_token = self.lexer.next_token();
		}

		// Parse the entirety of the program.
		pub fn parse_root_node(&mut self) -> ast::Root {
				let mut root = ast::Root {
						statements: Vec::new(),
				};

				while self.current_token != Token::EOF {
						let statement = self.parse_statement();
						if !statement.is_none() {
								root.statements.push(statement.unwrap());
						}

						self.next_token();
				}

				root
		}

		fn parse_statement(&mut self) -> Option<ast::Statement::Assigment> {
				match self.current_token {
						Token::Integer | Token::Float | Token::Char | Token::String => {
								self.parse_assigment_statement()
						}
						_ => None,
				}
		}

		fn parse_assigment_statement(&mut self) -> Option<ast::Statement> {
				let assigment_type = self.current_token.clone();

				match &self.current_token {
						Token::Identifier(_) => {}
						_ => return None,
				}

				let name: &String = match &self.current_token {
						Token::Identifier(value) => value,
						_ => return None,
				};

				while self.current_token != Token::Semicolon {
						self.next_token();
				}

				return Some(ast::Statement::Assigment(ast::AssigmentNode {
						value: (),
						variable_type: assigment_type,
						name: name.to_owned(),
				}));
		}

		fn expect_peek(&mut self, tok: Token) -> bool {
				if self.peek_token == tok {
						self.next_token();
						true
				} else {
						false
				}
		}
}

#[cfg(test)]
mod tests {
		use crate::scanner;

		use super::*;

		#[test]
		fn test_assignment_statements() {
				let input = "
int x = 5;
int y = 5;
int z = 123123;
";

				let lexer = scanner::Scanner::new(input);
				let mut parser = Parser::new(lexer);

				let root_node = parser.parse_root_node();
				assert_eq!(root_node.statements.len(), 3);
		}
}
