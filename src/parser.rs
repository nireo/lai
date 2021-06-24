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

    fn parse_infix(&mut self, lhs: Box<ast::Expression>) -> Option<ast::Expression> {
        match &self.current_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equals
            | Token::NEquals
            | Token::LessThan
            | Token::GreaterThan => self.parse_infix_expression(lhs),

            Token::LParen => self.parse_call_expression(lhs),
            _ => None,
        }
    }

    fn parse_prefix(&mut self, tok: Token) -> Option<ast::Expression> {
        match tok {
            Token::Identifier(_) => self.parse_identifier(),
            Token::Number(_) => self.parse_integer_literal(),
            Token::Exclamation | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => self.parse_boolean_literal(),

            Token::If => self.parse_if_expression(),
            Token::Fn => self.parse_function_literal(),

            _ => None,
        }
    }

    fn parse_call_expression(&mut self, func: Box<ast::Expression>) -> Option<ast::Expression> {
        let args = self.parse_call_arguments()?;

        Some(ast::Expression::FunctionCall(ast::FunctionCallNode {
            args,
            func,
        }))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<ast::Expression>> {
        let mut arguments: Vec<ast::Expression> = Vec::new();

        if self.peek_token == Token::RParen {
            self.next_token();
            return Some(arguments);
        }
        self.next_token();

        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();

            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.peek_token != Token::RParen {
            return None;
        }
        self.next_token();

        Some(arguments)
    }

    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        let identifier = match self.peek_token {
            Token::Identifier(_) => {
                self.next_token();

                self.parse_identifier()?
            }
            _ => return None,
        };

        if self.peek_token != Token::LParen {
            return None;
        }
        self.next_token();

        let function_parameters = self.parse_function_parameters()?;

        if self.peek_token != Token::Arrow {
            return None;
        }
        self.next_token();

        if !Parser::is_type_token(&self.peek_token) {
            return None;
        }
        self.next_token();
        let return_type = self.current_token.clone();

        if self.peek_token != Token::LBrace {
            return None;
        }
        self.next_token();

        let body = Box::new(self.parse_block_statement()?);

        Some(ast::Expression::Function(ast::FunctionNode {
            params: function_parameters,
            body,
            return_type,
            identifier: Box::new(identifier),
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<ast::Expression>> {
        let mut res: Vec<ast::Expression> = Vec::new();

        if self.peek_token == Token::RParen {
            self.next_token();
            return Some(res);
        }
        self.next_token();

        if !Parser::is_type_token(&self.current_token) {
            return None;
        }
        let first_param_type = self.current_token.clone();
        self.next_token();

        let first_param_name: String = match &self.current_token {
            Token::Identifier(value) => value.to_owned().clone(),
            _ => return None,
        };

        res.push(ast::Expression::FunctionParam(ast::FunctionParamNode {
            name: first_param_name,
            value_type: first_param_type,
        }));

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();

            let param_type = self.current_token.clone();
            self.next_token();

            let param_name: String = match &self.current_token {
                Token::Identifier(value) => value.to_owned().clone(),
                _ => return None,
            };

            res.push(ast::Expression::FunctionParam(ast::FunctionParamNode {
                name: param_name,
                value_type: param_type,
            }));
        }

        if self.peek_token != Token::RParen {
            return None;
        }
        self.next_token();

        Some(res)
    }

    pub fn is_type_token(tok: &Token) -> bool {
        match tok {
            Token::Void | Token::Integer | Token::String | Token::Char | Token::Float => true,
            _ => false,
        }
    }

    // if (<expr>) { <statement> } else { <statement> }
    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        if self.peek_token != Token::LParen {
            return None;
        }
        self.next_token();
        self.next_token();

        let cond = Box::new(self.parse_expression(Precedence::Lowest)?);

        if self.peek_token != Token::RParen {
            return None;
        }
        self.next_token();

        if self.peek_token != Token::LBrace {
            return None;
        }
        self.next_token();

        let after = Box::new(self.parse_block_statement()?);

        let other: Option<Box<ast::Statement>> = match self.peek_token {
            Token::Else => {
                self.next_token();

                if self.peek_token != Token::LBrace {
                    return None;
                }
                self.next_token();

                Some(Box::new(self.parse_block_statement()?))
            }
            _ => None,
        };

        Some(ast::Expression::If(ast::IfNode { other, after, cond }))
    }

    // { <statement> }
    fn parse_block_statement(&mut self) -> Option<ast::Statement> {
        let mut statements: Vec<ast::Statement> = Vec::new();
        self.next_token();

        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            let statement = self.parse_expression_statement()?;
            statements.push(statement);
            self.next_token();
        }

        Some(ast::Statement::Block(ast::BlockNode { statements }))
    }

    // <expr>
    fn parse_expression(&mut self, prec: Precedence) -> Option<ast::Expression> {
        let mut left = self.parse_prefix(self.current_token.clone())?;

        while self.peek_token != Token::Semicolon
            && Parser::precedence_to_num(&prec) < Parser::precedence_to_num(&self.peek_precedence())
        {
            self.next_token();
            left = self.parse_infix(Box::new(left))?;
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

    // 123123
    fn parse_integer_literal(&self) -> Option<ast::Expression> {
        let value: i32 = match &self.current_token {
            Token::Number(value) => value.parse().unwrap(),
            _ => return None,
        };

        Some(ast::Expression::Integer(ast::IntegerNode { value }))
    }

    // !<expr> -<expr>
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
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn current_precedence(&self) -> Precedence {
        self.token_to_precedence(&self.current_token)
    }

    fn peek_precedence(&self) -> Precedence {
        self.token_to_precedence(&self.peek_token)
    }

    fn parse_infix_expression(&mut self, left: Box<ast::Expression>) -> Option<ast::Expression> {
        let operator = self.current_token.clone();

        let prec = self.current_precedence();
        self.next_token();

        let rhs = self.parse_expression(prec)?;

        Some(ast::Expression::Infix(ast::InfixExpression {
            lhs: left,
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

    fn parse_boolean_literal(&self) -> Option<ast::Expression> {
        Some(ast::Expression::Boolean(ast::BooleanNode {
            value: self.current_token == Token::True,
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

    fn test_boolean_obj(exp: &ast::Expression, expected_value: bool) -> bool {
        match exp {
            ast::Expression::Boolean(value) => value.value == expected_value,
            _ => false,
        }
    }

    #[test]
    fn test_assignment_statements() {
        let input = "
    int x = 5;
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
    return 5;
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
        let input = "foobar;";

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
        let input = "5;";

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
                input: "!5;".to_owned(),
                operator: Token::Exclamation,
                value: 5,
            },
            PrefixTestcase {
                input: "-15;".to_owned(),
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
        struct InfixTestcase<T> {
            pub input: String,
            pub operator: Token,
            pub lhs: T,
            pub rhs: T,
        }

        let test_cases = vec![
            InfixTestcase {
                input: "5 + 5;".to_owned(),
                operator: Token::Plus,
                lhs: 5,
                rhs: 5,
            },
            InfixTestcase {
                input: "5 - 5;".to_owned(),
                operator: Token::Minus,
                lhs: 5,
                rhs: 5,
            },
            InfixTestcase {
                input: "5 * 5;".to_owned(),
                operator: Token::Asterisk,
                lhs: 5,
                rhs: 5,
            },
            InfixTestcase {
                input: "5 / 5;".to_owned(),
                operator: Token::Slash,
                lhs: 5,
                rhs: 5,
            },
            InfixTestcase {
                input: "5 > 5;".to_owned(),
                operator: Token::GreaterThan,
                lhs: 5,
                rhs: 5,
            },
            InfixTestcase {
                input: "5 < 5;".to_owned(),
                operator: Token::LessThan,
                lhs: 5,
                rhs: 5,
            },
            InfixTestcase {
                input: "5 != 5;".to_owned(),
                operator: Token::NEquals,
                lhs: 5,
                rhs: 5,
            },
            InfixTestcase {
                input: "5 == 5;".to_owned(),
                operator: Token::Equals,
                lhs: 5,
                rhs: 5,
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

    #[test]
    fn boolean_literal_test() {
        struct BooleanTestcase {
            pub input: String,
            pub expected_val: bool,
        }

        let test_cases = vec![
            BooleanTestcase {
                input: "true;".to_owned(),
                expected_val: true,
            },
            BooleanTestcase {
                input: "true;".to_owned(),
                expected_val: true,
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
                        ast::Expression::Boolean(bol) => {
                            assert_eq!(bol.value, tc.expected_val);
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
    fn test_boolean_infix() {
        struct InfixTestcase {
            pub input: String,
            pub operator: Token,
            pub lhs: bool,
            pub rhs: bool,
        }

        let test_cases = vec![
            InfixTestcase {
                input: "true == true ;".to_owned(),
                operator: Token::Equals,
                lhs: true,
                rhs: true,
            },
            InfixTestcase {
                input: "true != false ;".to_owned(),
                operator: Token::NEquals,
                lhs: true,
                rhs: false,
            },
            InfixTestcase {
                input: "false == false ;".to_owned(),
                operator: Token::Equals,
                lhs: false,
                rhs: false,
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
                            assert!(test_boolean_obj(&*infix.rhs, tc.rhs));
                            assert!(test_boolean_obj(&*infix.lhs, tc.lhs));
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
    fn test_if_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = scanner::Scanner::new(&input);
        let mut parser = Parser::new(lexer);

        let root_node = parser.parse_root_node();
        assert_eq!(root_node.statements.len(), 1);

        let is_correct_type = match &root_node.statements[0] {
            ast::Statement::Expression(val) => {
                let to_return = match &val.value {
                    ast::Expression::If(exp) => {
                        if exp.other.is_none() {
                            false
                        } else {
                            true
                        }
                    }
                    _ => false,
                };

                to_return
            }
            _ => false,
        };

        assert!(is_correct_type);
    }

    #[test]
    fn test_function_literal_no_params() {
        let input = "fn func() -> int { 1 }";
        let lexer = scanner::Scanner::new(&input);
        let mut parser = Parser::new(lexer);

        let root_node = parser.parse_root_node();
        assert_eq!(root_node.statements.len(), 1);

        let is_correct_type = match &root_node.statements[0] {
            ast::Statement::Expression(val) => {
                let to_return = match &val.value {
                    ast::Expression::Function(exp) => {
                        if exp.return_type != Token::Integer {
                            false
                        } else {
                            true
                        }
                    }
                    _ => false,
                };

                to_return
            }
            _ => false,
        };

        assert!(is_correct_type);
    }

    #[test]
    fn test_function_literal_with_params() {
        struct ParamTest {
            pub name: String,
            pub param_type: Token,
        }

        let expected = vec![
            ParamTest {
                name: "x".to_owned(),
                param_type: Token::Integer,
            },
            ParamTest {
                name: "y".to_owned(),
                param_type: Token::Float,
            },
            ParamTest {
                name: "z".to_owned(),
                param_type: Token::String,
            },
        ];

        let input = "fn func(int x, float y, string z) -> int { 1 }";
        let lexer = scanner::Scanner::new(&input);
        let mut parser = Parser::new(lexer);

        let root_node = parser.parse_root_node();
        assert_eq!(root_node.statements.len(), 1);

        let is_correct_type = match &root_node.statements[0] {
            ast::Statement::Expression(val) => {
                let to_return = match &val.value {
                    ast::Expression::Function(exp) => {
                        if exp.return_type != Token::Integer {
                            false
                        } else {
                            let mut ok = true;
                            for (i, param) in exp.params.iter().enumerate() {
                                match param {
                                    ast::Expression::FunctionParam(node) => {
                                        assert!(expected[i].name == node.name);
                                        assert!(expected[i].param_type == node.value_type);
                                    }
                                    _ => ok = false,
                                }
                            }

                            assert!(ok);
                            true
                        }
                    }
                    _ => false,
                };

                to_return
            }
            _ => false,
        };

        assert!(is_correct_type);
    }

    #[test]
    fn call_expressions() {
        let input = "equals(true, false);";
        let lexer = scanner::Scanner::new(&input);
        let mut parser = Parser::new(lexer);

        let root_node = parser.parse_root_node();
        assert_eq!(root_node.statements.len(), 1);

        let is_correct_type = match &root_node.statements[0] {
            ast::Statement::Expression(val) => {
                let to_return = match &val.value {
                    ast::Expression::FunctionCall(exp) => exp.args.len() == 2,
                    _ => false,
                };

                to_return
            }
            _ => false,
        };

        assert!(is_correct_type);
    }
}
