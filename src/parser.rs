use crate::ast;
pub use crate::scanner::*;

pub struct Parser {
		pub lexer: Scanner,

		current_token: Token,
		peek_token: Token,
}

impl Parser {
		pub fn new(mut lexer: Scanner) -> Self {
				let current_token = lexer.next_token();
				let peek_token = lexer.next_token();

				Self {
						lexer,
						current_token,
						peek_token,
				}
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
								println!("appended statement...");
								root.statements.push(statement.unwrap());
						}

						self.next_token();
				}

				root
		}

		fn parse_statement(&mut self) -> Option<ast::Statement> {
				match self.current_token {
						Token::Integer | Token::Float | Token::Char | Token::String => {
								self.parse_assigment_statement()
						}
						_ => None,
				}
		}

		fn parse_assigment_statement(&mut self) -> Option<ast::Statement> {
				let assigment_type = self.current_token.clone();

				self.next_token();
				match &self.current_token {
						Token::Identifier(_) => {}
						_ => return None,
				}

				let name: String = match &self.current_token {
						Token::Identifier(value) => value.to_owned().clone(),
						_ => return None,
				};

				self.next_token();

				match &self.current_token {
						Token::Assign => {}
						_ => return None,
				}

				while self.current_token != Token::Semicolon {
						self.next_token();
				}

				println!("parsing statement...3");

				return Some(ast::Statement::Assigment(ast::AssigmentNode {
						value: ast::Expression::NonExisting,
						variable_type: assigment_type,
						name,
				}));
		}
}

#[cfg(test)]
mod tests {
		use crate::scanner;

		use super::*;

		#[test]
		fn test_assignment_statements() {
				let input = "
		int x = 5 ;
		";

				let lexer = scanner::Scanner::new(input);
				let mut parser = Parser::new(lexer);

				let root_node = parser.parse_root_node();
				assert_eq!(root_node.statements.len(), 1);
		}
}
