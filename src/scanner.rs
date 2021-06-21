#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Keywords
    If,
    Else,
    Return,
    String,
    Integer,
    Float,
    Char,
    Illegal,
    Identifier(String),
    Number(String),
    True,
    False,

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
    Exclamation,

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

pub struct Scanner {
    input: String,
    pos: usize,
    ch: u8,
    read_pos: usize,
}

impl Scanner {
    pub fn new(input_str: &str) -> Self {
        let mut res = Self {
            read_pos: 0,
            ch: 0,
            input: input_str.to_string(),
            pos: 0,
        };

        res.read_char();
        res
    }

    fn check_if_keyword(&self, keyword: &str) -> Token {
        let token_type = match keyword {
            "if" => Token::If,
            "else" => Token::Else,
            "float" => Token::Float,
            "char" => Token::Char,
            "string" => Token::String,
            "int" => Token::Integer,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Identifier(keyword.to_string()),
        };
        token_type
    }

    fn read_char(&mut self) {
        self.ch = {
            if self.read_pos >= self.input.len() {
                0
            } else {
                self.input.as_bytes()[self.read_pos]
            }
        };

        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn read_identifier(&mut self) -> String {
        let start_pos: usize = self.pos;
        loop {
            match self.ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.read_char(),
                _ => break,
            }
        }

        std::str::from_utf8(&self.input.as_bytes()[start_pos..self.pos])
            .unwrap()
            .to_string()
    }

    fn read_number(&mut self) -> String {
        let start_pos: usize = self.pos;
        loop {
            match self.ch {
                b'0'..=b'9' => self.read_char(),
                _ => break,
            }
        }

        std::str::from_utf8(&self.input.as_bytes()[start_pos..self.pos])
            .unwrap()
            .to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char()
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Equals
                } else {
                    Token::Assign
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NEquals
                } else {
                    Token::Exclamation
                }
            }
            b'+' => Token::Plus,
            b'*' => Token::Asterisk,
            b'-' => Token::Minus,
            b'/' => Token::Slash,
            b'%' => Token::Modulo,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'.' => Token::Dot,
            b'<' => Token::LessThan,
            b'>' => Token::GreaterThan,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let keyword = self.read_identifier();
                return self.check_if_keyword(&keyword);
            }
            b'0'..=b'9' => {
                let num = self.read_number();
                Token::Number(num)
            }
            0 => Token::EOF,
            _ => Token::Illegal,
        };

        self.read_char();

        token
    }

    fn peek_char(&self) -> u8 {
        if self.read_pos >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_pos]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_characters() {
        let input = "=+(){}[],;.!";
        let mut lexer = Scanner::new(input);

        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::LBracket,
            Token::RBracket,
            Token::Comma,
            Token::Semicolon,
            Token::Dot,
            Token::Exclamation,
            Token::EOF,
        ];

        for expected_token in expected.iter() {
            let actual_token = lexer.next_token();
            assert_eq!(*expected_token, actual_token);
        }
    }

    #[test]
    fn keywords() {
        let input = "int float if else";

        let mut lexer = Scanner::new(input);

        let expected = vec![Token::Integer, Token::Float, Token::If, Token::Else];

        for expected_token in expected.iter() {
            let actual_token = lexer.next_token();
            assert_eq!(*expected_token, actual_token);
        }
    }

    #[test]
    fn not_equals_equals() {
        let input = "!= ==";
        let mut lexer = Scanner::new(input);

        let expected = vec![Token::NEquals, Token::Equals];

        for expected_token in expected.iter() {
            let actual_token = lexer.next_token();
            assert_eq!(*expected_token, actual_token);
        }
    }

    #[test]
    fn numbers() {
        let input = "123123";

        let mut lexer = Scanner::new(input);
        let token = lexer.next_token();

        match token {
            Token::Number(value) => assert_eq!(value, "123123"),
            _ => assert!(false), // XD,
        }
    }

    #[test]
    fn let_statement() {
        let input = "int x = 10 ;";

        let mut lexer = Scanner::new(input);

        let expected = vec![
            Token::Integer,
            Token::Identifier("x".to_owned()),
            Token::Assign,
            Token::Number("10".to_owned()),
            Token::Semicolon,
            Token::EOF,
        ];

        for expected_token in expected.iter() {
            let actual_token = lexer.next_token();
            assert_eq!(*expected_token, actual_token);
        }
    }
}
