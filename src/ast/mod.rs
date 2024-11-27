use crate::token::{Token, TokenType};

pub trait TokenLiteral<'a> {
    fn token_literal(&self) -> &'a str;
}

struct Node {}

pub struct Statement<'a> {
    pub literal: &'a str,
}

struct Expression {}

struct LetStatement<'a> {
    token: TokenType,
    name: Identifier<'a>,
    value: Expression,
}

impl<'a> TokenLiteral<'a> for Statement<'a> {
    fn token_literal(&self) -> &'a str {
        self.literal
    }
}

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}
//
//impl<'a> Program<'a> {
//    fn new(&self) -> Self {
//        if self.statements.len() > 0 {
//            self.statements[0].token_literal()
//        } else {
//            ""
//        }
//    }
//}

struct Identifier<'a> {
    token: TokenType,
    value: &'a str,
}

impl<'a> TokenLiteral<'a> for Identifier<'a> {
    fn token_literal(&self) -> &'a str {
        self.value
    }
}
