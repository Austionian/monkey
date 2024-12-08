use crate::token::Token;
use std::fmt::{Debug, Display};

pub trait TokenLiteral {
    fn token_literal(&self) -> String;
}

struct Node {}

#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressStatement(ExpressionStatement),
}

#[derive(Default, Debug)]
pub enum Expression {
    PrefixExpression((Token, Box<ExpressionStatement>)),
    IdentExpression(Token),
    IntExpression(Token),
    #[default]
    UnknownExpression,
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Token,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, Default)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::new();
        for s in self.statements.iter() {
            buffer.push_str(&s.to_string());
        }

        write!(f, "{buffer}")
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::new();
        match self {
            Self::LetStatement(s) => buffer.push_str(&s.to_string()),
            Self::ReturnStatement(s) => buffer.push_str(&s.to_string()),
            Self::ExpressStatement(s) => buffer.push_str(&s.to_string()),
        };

        write!(f, "{buffer}")
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = ;",
            self.token.token_literal(),
            self.name.token_literal()
        )
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ;", self.token.token_literal())
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.token_literal())
    }
}

impl Program {
    fn new() -> Self {
        Program { statements: vec![] }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        let mut program = Program::new();

        program
            .statements
            .push(Statement::LetStatement(LetStatement {
                token: Token::LET,
                name: Token::IDENT("test".to_string()),
                value: Expression::default(),
            }));

        assert_eq!(program.to_string(), "let test = ;");
    }
}
