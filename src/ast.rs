use crate::scanner::Token;

// The usage of box is only in some ast nodes in which the fields use recursive struct types.
// For example: statements needing a statement inside of it. Box<T> allows this to happen.

#[derive(Debug, Clone)]
pub enum Node {
    Root(Box<Root>),
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(IntegerNode),
    Char(CharNode),
    Boolean(BooleanNode),
    Float(FloatNode),
    String(StringNode),
    Function(FunctionNode),
    Identifier(IdentifierNode),
    FunctionCall(FunctionCallNode),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfNode),
    FunctionParam(FunctionParamNode),
    Array(ArrayNode),
    Index(IndexExpression),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assigment(AssigmentNode),
    Return(ReturnNode),
    Block(BlockNode),
    Expression(ExpressionStatementNode),
    Function(FunctionNode),
}

#[derive(Debug, Clone)]
pub struct Root {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FunctionParamNode {
    pub value_type: Token,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct AssigmentNode {
    pub variable_type: Token,
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub operator: Token,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub operator: Token,
    pub rhs: Box<Expression>,
    pub lhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    // the other statement doesn't need to exist
    pub other: Option<Box<Statement>>,
    pub after: Box<Statement>,
    pub cond: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub params: Vec<Expression>,
    pub body: Box<Statement>,
    pub return_type: Token,
    pub identifier: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallNode {
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct IntegerNode {
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct BooleanNode {
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct FloatNode {
    pub value: f32,
}

#[derive(Debug, Clone)]
pub struct CharNode {
    pub value: char,
}

#[derive(Debug, Clone)]
pub struct StringNode {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatementNode {
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct IdentifierNode {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct ArrayNode {
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub lhs: Box<Expression>,
    pub index: Box<Expression>,
}
