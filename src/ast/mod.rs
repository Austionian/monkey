use crate::token::Token;

pub trait TokenLiteral {
    fn token_literal(&self) -> String;
}

struct Node {}

#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
}

#[derive(Default, Debug)]
pub struct Expression {}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Token,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    fn new(&self) -> Self {
        Program { statements: vec![] }
    }
}
