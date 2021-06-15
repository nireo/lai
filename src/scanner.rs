#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
		// Keywords
		If,
		Else,
		Return,
		Identifier,

		// braces, brackets and parenthesis
		LParen,
		RParen,
		LBrace,
		RBrace,
		LBracket,
		RBracket,

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
}

struct Scanner {
		input: &str,
		pos: usize,
		ch: char,
		read_pos: usize,
}

impl Scanner {
		pub fn new(input: &str) -> Scanner {}

		fn is_letter(ch: char) -> bool {
				('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
		}

		fn read_char(&mut self) {
				if (self.read_pos >= input.len()) {
						self.ch = 0;
				} else {
						ch = input.chars().nth(self.read_pos)
				}

				self.pos = self.read_pos;
				self.read_pos += 1;
		}

		fn next_token() -> Token {}
		fn skip_whitespace() {}
		fn peek_char() -> char {}
}
