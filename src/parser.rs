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

		fn parse_prefix(&mut self, tok: Token) -> Option<ast::Expression> {
				match tok {
						Token::Identifier(_) => self.parse_identifier(),
						Token::Number(_) => self.parse_integer_literal(),
						Token::Exclamation => self.parse_prefix_expression(),
						Token::Minus => self.parse_prefix_expression(),
						_ => None,
				}
		}

		fn parse_expression(&mut self, prec: Precedence) -> Option<ast::Expression> {
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

		fn parse_integer_literal(&self) -> Option<ast::Expression> {
				let value: i32 = match &self.current_token {
						Token::Number(value) => value.parse().unwrap(),
						_ => return None,
				};

				Some(ast::Expression::Integer(ast::IntegerNode { value }))
		}

		fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
				let operator = self.current_token.clone();

				self.next_token();

				let right = self.parse_expression(Precedence::Prefix);

				if right.is_none() {
						return None;
				}

				Some(ast::Expression::Prefix(ast::PrefixExpression {
						operator,
						rhs: Box::new(right.unwrap()),
				}))
		}
}

#[cfg(test)]
mod tests {
		use crate::scanner;

		use super::*;

		fn test_integer_obj(exp: &ast::Expression, expected_value: i32) -> bool {
				match exp {
						ast::Expression::Integer(value) => value.value == expected_value,
						_ => false,
				}
		}

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

		#[test]
		fn test_identifier_expression() {
				let input = "foobar ;";

				let lexer = scanner::Scanner::new(input);
				let mut parser = Parser::new(lexer);

				let root_node = parser.parse_root_node();
				assert_eq!(root_node.statements.len(), 1);

				let is_identifier_type = match &root_node.statements[0] {
						ast::Statement::Expression(val) => {
								let to_return = match val.value {
										ast::Expression::Identifier(_) => true,
										_ => false,
								};

								to_return
						}
						_ => false,
				};

				assert!(is_identifier_type);
		}

		#[test]
		fn test_integer_literal() {
				let input = "5 ;";

				let lexer = scanner::Scanner::new(input);
				let mut parser = Parser::new(lexer);

				let root_node = parser.parse_root_node();
				assert_eq!(root_node.statements.len(), 1);

				let is_correct_type = match &root_node.statements[0] {
						ast::Statement::Expression(val) => {
								let to_return = match val.value {
										ast::Expression::Integer(_) => true,
										_ => false,
								};

								to_return
						}
						_ => false,
				};

				assert!(is_correct_type);
		}

		#[test]
		fn prefix_parsing() {
				struct PrefixTestcase {
						pub input: String,
						pub operator: Token,
						pub value: i32,
				}

				let test_cases = vec![
						PrefixTestcase {
								input: "!5 ;".to_owned(),
								operator: Token::Exclamation,
								value: 5,
						},
						PrefixTestcase {
								input: "-15 ;".to_owned(),
								operator: Token::Minus,
								value: 15,
						},
				];

				for tc in test_cases.iter() {
						let lexer = scanner::Scanner::new(&tc.input);
						let mut parser = Parser::new(lexer);

						let root_node = parser.parse_root_node();
						assert_eq!(root_node.statements.len(), 1);

						let is_correct_type = match &root_node.statements[0] {
								ast::Statement::Expression(val) => {
										let to_return = match &val.value {
												ast::Expression::Prefix(prefix) => {
														assert_eq!(prefix.operator, tc.operator);
														assert!(test_integer_obj(&*prefix.rhs, tc.value));
														true
												}
												_ => false,
										};

										to_return
								}
								_ => false,
						};

						assert!(is_correct_type);
				}
		}
}
