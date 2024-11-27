use crate::token::{Token, TokenType};

pub trait TokenLiteral {
    fn token_literal(&self) -> &str;
}

struct Node {}

pub enum Statement {
    LetStatement(LetStatement),
}

#[derive(Default, Debug)]
pub struct Expression {}

#[derive(Debug)]
pub struct LetStatement {
    pub token: TokenType,
    pub name: Identifier,
    pub value: Expression,
}

impl TokenLiteral for LetStatement {
    fn token_literal(&self) -> &str {
        &self.name.value
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    fn new(&self) -> Self {
        Program { statements: vec![] }
    }
}

#[derive(Default, Debug)]
pub struct Identifier {
    pub token: TokenType,
    pub value: String,
}

impl TokenLiteral for Identifier {
    fn token_literal(&self) -> &str {
        &self.value
    }
}
