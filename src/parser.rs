use crate::ast;
pub use crate::scanner::*;

pub struct Parser {
		pub lexer: Scanner,

		current_token: Token,
		peek_token: Token,
}

pub enum Precedence {
		Lowest,
		Equals,
		LT,
		Sum,
		Product,
		Prefix,
		Call,
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

		fn parse_infix(&mut self, tok: Token) -> Option<ast::Expression> {
				match tok {
						_ => None,
				}
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
				let mut left = self.parse_prefix(self.current_token.clone())?;

				while self.peek_token != Token::Semicolon
						&& Parser::precedence_to_num(&prec)
								< Parser::precedence_to_num(&self.current_precedence())
				{
						left = self.parse_infix(self.peek_token.clone())?;
				}

				Some(left)
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
				let right = self.parse_expression(Precedence::Prefix)?;

				Some(ast::Expression::Prefix(ast::PrefixExpression {
						operator,
						rhs: Box::new(right),
				}))
		}

		fn token_to_precedence(&self, token: &Token) -> Precedence {
				match token {
						Token::Equals => Precedence::Equals,
						Token::NEquals => Precedence::Equals,
						Token::LessThan => Precedence::LT,
						Token::GreaterThan => Precedence::LT,
						Token::Plus => Precedence::Sum,
						Token::Minus => Precedence::Sum,
						Token::Slash => Precedence::Product,
						Token::Asterisk => Precedence::Product,
						_ => Precedence::Lowest,
				}
		}

		fn current_precedence(&self) -> Precedence {
				self.token_to_precedence(&self.current_token)
		}

		fn peek_precedence(&self) -> Precedence {
				self.token_to_precedence(&self.peek_token)
		}

		fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
				let operator = self.current_token.clone();

				let prec = self.current_precedence();
				self.next_token();

				let rhs = self.parse_expression(prec)?;

				Some(ast::Expression::Infix(ast::InfixExpression {
						lhs: Box::new(left),
						rhs: Box::new(rhs),
						operator,
				}))
		}

		pub fn precedence_to_num(prec: &Precedence) -> i32 {
				match prec {
						Precedence::Lowest => 0,
						Precedence::Equals => 1,
						Precedence::LT => 2,
						Precedence::Sum => 3,
						Precedence::Product => 4,
						Precedence::Prefix => 5,
						Precedence::Call => 6,
				}
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

		#[test]
		fn test_parsing_infix_expression() {
				struct InfixTestcase {
						pub input: String,
						pub operator: Token,
				}

				let test_cases = vec![
						InfixTestcase {
								input: "5 + 5 ;".to_owned(),
								operator: Token::Plus,
						},
						InfixTestcase {
								input: "5 - 5 ;".to_owned(),
								operator: Token::Minus,
						},
						InfixTestcase {
								input: "5 * 5 ;".to_owned(),
								operator: Token::Asterisk,
						},
						InfixTestcase {
								input: "5 / 5 ;".to_owned(),
								operator: Token::Slash,
						},
						InfixTestcase {
								input: "5 > 5 ;".to_owned(),
								operator: Token::GreaterThan,
						},
						InfixTestcase {
								input: "5 < 5 ;".to_owned(),
								operator: Token::LessThan,
						},
						InfixTestcase {
								input: "5 != 5 ;".to_owned(),
								operator: Token::NEquals,
						},
						InfixTestcase {
								input: "5 == 5 ;".to_owned(),
								operator: Token::Equals,
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
												ast::Expression::Infix(infix) => {
														assert_eq!(infix.operator, tc.operator);
														assert!(test_integer_obj(&*infix.rhs, 5));
														assert!(test_integer_obj(&*infix.lhs, 5));
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
