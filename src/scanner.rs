use std::collections::HashMap;

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

		// braces, brackets and parenthesis
		LParen,
		RParen,
		LBrace,
		RBrace,
		LBracket,
		RBracket,
		Semicolon,
		Comma,

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
		token_type: TokenType,
		lit: String,
}

pub struct Scanner {
		input: String,
		pos: usize,
		ch: char,
		read_pos: usize,

		keywords: HashMap<String, TokenType>,
}

impl Scanner {
		pub fn new(input_str: &str) -> Self {
				let res = Self {
						read_pos: 0,
						ch: '\0',
						input: input_str.to_string(),
						pos: 0,
						keywords: HashMap::new(),
				};

				res.keywords.insert("return".to_string(), TokenType::Return);
				res.keywords.insert("int".to_string(), TokenType::Return);
				res.keywords.insert("float".to_string(), TokenType::Return);
				res.keywords.insert("str".to_string(), TokenType::Return);
				res.keywords.insert("char".to_string(), TokenType::Char);

				res.read_char();
				res
		}

		fn is_letter(&self, ch: char) -> bool {
				('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
		}

		fn check_if_keyword(&self, keyword: String) -> TokenType {
				if let Some(value) = self.keywords.get(&keyword) {
						value.clone()
				} else {
						TokenType::Identifier
				}
		}

		fn read_char(&mut self) {
				match self.input.chars().nth(self.read_pos) {
						None => self.ch = '\0',
						Some(ch) => self.ch = ch,
				}

				self.pos = self.read_pos;
				self.read_pos += 1;
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
						_ => {
								if self.is_letter(self.ch) {
										// read identifier
										let word = self.read_identifier();
										return Token {
												token_type: self.check_if_keyword(word),
												lit: word.to_string(),
										};
								} else if self.ch.is_numeric() {
										// read number
										let number = self.read_number();
										return Token {
												token_type: TokenType::Number,
												lit: number.to_string(),
										};
								}

								Token {
										token_type: TokenType::Identifier,
										lit: self.ch.to_string(),
								}
						}
				};

				token
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
}
