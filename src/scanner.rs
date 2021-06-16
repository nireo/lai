#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
		// Keywords
		If,
		Else,
		Return,
		Identifier,
		String,
		Integer,
		Float,
		Char,
		Number,
		Illegal,

		// braces, brackets and parenthesis
		LParen,
		RParen,
		LBrace,
		RBrace,
		LBracket,
		RBracket,
		Semicolon,
		Comma,
		Dot,

		// equality tokens
		Equals,
		NEquals,
		GreaterThan,
		LessThan,

		// Operators
		Asterisk,
		Plus,
		Minus,
		Slash,
		Modulo,
		Assign,

		EOF,
}

pub struct Token {
		pub token_type: TokenType,
		pub lit: String,
}

pub struct Scanner {
		input: String,
		pos: usize,
		ch: char,
		read_pos: usize,
}

impl Scanner {
		pub fn new(input_str: &str) -> Self {
				let mut res = Self {
						read_pos: 0,
						ch: '\0',
						input: input_str.to_string(),
						pos: 0,
				};

				res.read_char();
				res
		}

		fn is_letter(&self, ch: char) -> bool {
				('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
		}

		fn check_if_keyword(&self, keyword: &str) -> TokenType {
				let token_type = match keyword {
						"if" => TokenType::If,
						"else" => TokenType::Else,
						"float" => TokenType::Float,
						"char" => TokenType::Char,
						"string" => TokenType::String,
						"int" => TokenType::Integer,
						_ => TokenType::Identifier,
				};
				token_type
		}

		fn read_char(&mut self) {
				match self.input.chars().nth(self.read_pos) {
						None => self.ch = '\0',
						Some(ch) => self.ch = ch,
				}

				self.pos = self.read_pos;
				self.read_pos += 1;
		}

		fn read_identifier(&mut self) -> String {
				let start_pos: usize = self.pos;
				while self.is_letter(self.ch) {
						self.read_char();
				}

				return self.input[start_pos..self.pos - start_pos].to_string();
		}

		fn read_number(&mut self) -> String {
				let start_pos: usize = self.pos;
				while self.ch.is_numeric() {
						self.read_char();
				}

				return self.input[start_pos..self.pos - start_pos].to_string();
		}

		fn skip_whitespace(&mut self) {
				while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
						self.read_char()
				}
		}

		pub fn next_token(&mut self) -> Token {
				self.skip_whitespace();

				let token = match self.ch {
						'=' => Token {
								token_type: TokenType::Assign,
								lit: self.ch.to_string(),
						},
						';' => Token {
								token_type: TokenType::Semicolon,
								lit: self.ch.to_string(),
						},
						'}' => Token {
								token_type: TokenType::RBrace,
								lit: self.ch.to_string(),
						},
						'{' => Token {
								token_type: TokenType::LBrace,
								lit: self.ch.to_string(),
						},
						'[' => Token {
								token_type: TokenType::LBracket,
								lit: self.ch.to_string(),
						},
						']' => Token {
								token_type: TokenType::RBracket,
								lit: self.ch.to_string(),
						},
						'(' => Token {
								token_type: TokenType::LParen,
								lit: self.ch.to_string(),
						},
						')' => Token {
								token_type: TokenType::RParen,
								lit: self.ch.to_string(),
						},
						'+' => Token {
								token_type: TokenType::Plus,
								lit: self.ch.to_string(),
						},
						'-' => Token {
								token_type: TokenType::Minus,
								lit: self.ch.to_string(),
						},
						'/' => Token {
								token_type: TokenType::Slash,
								lit: self.ch.to_string(),
						},
						'*' => Token {
								token_type: TokenType::Asterisk,
								lit: self.ch.to_string(),
						},
						'%' => Token {
								token_type: TokenType::Modulo,
								lit: self.ch.to_string(),
						},
						'<' => Token {
								token_type: TokenType::LessThan,
								lit: self.ch.to_string(),
						},
						'>' => Token {
								token_type: TokenType::GreaterThan,
								lit: self.ch.to_string(),
						},
						'"' => Token {
								token_type: TokenType::String,
								lit: self.ch.to_string(),
						},
						'\0' => Token {
								token_type: TokenType::EOF,
								lit: self.ch.to_string(),
						},
						',' => Token {
								token_type: TokenType::Comma,
								lit: self.ch.to_string(),
						},
						'.' => Token {
								token_type: TokenType::Dot,
								lit: self.ch.to_string(),
						},
						_ => {
								if self.is_letter(self.ch) {
										// read identifier
										let word = self.read_identifier();
										return Token {
												token_type: self.check_if_keyword(&word),
												lit: word,
										};
								} else if self.ch.is_numeric() {
										// read number
										let number = self.read_number();
										return Token {
												token_type: TokenType::Number,
												lit: number.to_string(),
										};
								} else {
										let token = Token {
												token_type: TokenType::Illegal,
												lit: self.ch.to_string(),
										};
										self.read_char();
										return token;
								}
						}
				};

				self.read_char();

				token
		}
}

#[cfg(test)]
mod tests {
		use super::*;

		#[test]
		fn basic_characters() {
				let input = "=+(){}[],;.";
				let mut lexer = Scanner::new(input);

				let expected = vec![
						TokenType::Assign,
						TokenType::Plus,
						TokenType::LParen,
						TokenType::RParen,
						TokenType::LBrace,
						TokenType::RBrace,
						TokenType::LBracket,
						TokenType::RBracket,
						TokenType::Comma,
						TokenType::Semicolon,
						TokenType::Dot,
						TokenType::EOF,
				];

				for expected_token in expected.iter() {
						let actual_token = lexer.next_token();
						assert_eq!(*expected_token, actual_token.token_type);
				}
		}

		#[test]
		fn keywords() {
				let input = "int float if else";

				let mut lexer = Scanner::new(input);

				let expected = vec![
						TokenType::Integer,
						TokenType::Float,
						TokenType::If,
						TokenType::Else,
				];

				for expected_token in expected.iter() {
						let actual_token = lexer.next_token();
						println!("{}", actual_token.lit);
						assert_eq!(*expected_token, actual_token.token_type);
				}
		}
}
