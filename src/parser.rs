use crate::ast;
pub use crate::scanner::*;

pub struct Parser {
		pub lexer: Scanner,

		current_token: Token,
		peek_token: Token,
}

enum Precedence {
		Lowest = 0,
		Equals = 1,
		LT = 2,
		Sum = 3,
		Product = 4,
		Prefix = 5,
		Call = 6,
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
						Token::Return => self.parse_return_statement(),
						_ => self.parse_expression_statement(),
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

				return Some(ast::Statement::Assigment(ast::AssigmentNode {
						value: ast::Expression::NonExisting,
						variable_type: assigment_type,
						name,
				}));
		}

		fn parse_return_statement(&mut self) -> Option<ast::Statement> {
				self.next_token();

				while self.current_token != Token::Semicolon {
						self.next_token();
				}

				return Some(ast::Statement::Return(ast::ReturnNode {
						value: ast::Expression::NonExisting,
				}));
		}

		fn parse_identifier(&self) -> Option<ast::Expression> {
				let name: String = match &self.current_token {
						Token::Identifier(value) => value.to_owned().clone(),
						_ => return None,
				};

				return Some(ast::Expression::Identifier(ast::IdentifierNode { name }));
		}

		fn parse_prefix(&self, tok: Token) -> Option<ast::Expression> {
				match tok {
						Token::Identifier(_) => self.parse_identifier(),
						_ => None,
				}
		}

		fn parse_expression(&self, prec: Precedence) -> Option<ast::Expression> {
				self.parse_prefix(self.current_token.clone())
		}

		fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
				let expression = match self.parse_expression(Precedence::Lowest) {
						Some(val) => val,
						_ => return None,
				};

				Some(ast::Statement::Expression(ast::ExpressionStatementNode {
						value: expression,
				}))
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

				match &root_node.statements[0] {
						ast::Statement::Assigment(assigment_struct) => {
								assert_eq!(assigment_struct.name, "x".to_owned());
								assert_eq!(assigment_struct.variable_type, Token::Integer);
						}
						_ => assert!(false),
				}
		}

		#[test]
		fn test_return_statements() {
				let input = "
		return 5 ;
		";

				let lexer = scanner::Scanner::new(input);
				let mut parser = Parser::new(lexer);

				let root_node = parser.parse_root_node();
				assert_eq!(root_node.statements.len(), 1);

				let is_return_type = match &root_node.statements[0] {
						ast::Statement::Return(_) => true,
						_ => false,
				};

				assert!(is_return_type);
		}
}
