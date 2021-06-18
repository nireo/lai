use crate::scanner::Token;

// The usage of box is only in some ast nodes in which the fields use recursive struct types.
// For example: statements needing a statement inside of it. Box<T> allows this to happen.

#[derive(Debug)]
pub enum Expression {
		Integer(IntegerNode),
		Char(CharNode),
		Boolean(BooleanNode),
		Float(FloatNode),
		String(StringNode),
		Function(FunctionNode),
		FunctionCall(FunctionCallNode),
}

#[derive(Debug)]
pub enum Statement {
		Assigment(AssigmentNode),
		If(IfNode),
		Block(BlockNode),
		Expression(ExpressionStatementNode),
}

#[derive(Debug)]
pub struct Root {
		pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct AssigmentNode {
		pub variable_type: Token,
		pub name: String,
		pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnNode {
		pub value: Expression,
}

#[derive(Debug)]
pub struct IfNode {
		pub value: Expression,

		// the other statement doesn't need to exist
		pub other: Option<Box<Statement>>,
		pub after: Box<Statement>,
		pub cond: Expression,
}

#[derive(Debug)]
pub struct FunctionNode {
		pub params: Vec<Expression>,
		pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct FunctionCallNode {
		pub func: Box<Expression>,
		pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct BlockNode {
		pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct IntegerNode {
		pub value: i32,
}

#[derive(Debug)]
pub struct BooleanNode {
		pub value: bool,
}

#[derive(Debug)]
pub struct FloatNode {
		pub value: f32,
}

#[derive(Debug)]
pub struct CharNode {
		pub value: char,
}

#[derive(Debug)]
pub struct StringNode {
		pub value: String,
}

#[derive(Debug)]
pub struct ExpressionStatementNode {
		pub value: Expression,
}

#[derive(Debug)]
pub struct IdentifierNode {
		pub name: String,
}
