use crate::lexer::Token;

pub enum Node {
    Program(Program),
    Statement,
    Expression,
}

pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
    Int(i64),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Expression,
}

#[derive(Debug)]
pub struct InfixExpression {
    pub left: Expression,
    pub operator: Token,
    pub right: Expression,
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}
