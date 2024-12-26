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

#[derive(Debug)]
pub enum Expression {
    // Token ie prefix, Right
    PrefixExpression((Token, Box<ExpressionStatement>)),
    // Token, Left, Right
    InfixExpression((Token, Box<ExpressionStatement>, Box<ExpressionStatement>)),
    IdentExpression(Token),
    IntExpression(Token),
    BoolExpression(Token),
    // Token, condition, consequence, alternative
    IfExpression(
        Token,
        Box<ExpressionStatement>,
        Box<BlockStatement>,
        Option<Box<BlockStatement>>,
    ),
    UnknownExpression(Token),
}

impl Default for Expression {
    fn default() -> Self {
        Expression::UnknownExpression(Token::default())
    }
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

#[derive(Debug, Default)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::new();
        for s in self.statements.iter() {
            buffer.push_str(&s.to_string());
        }
        write!(f, "{buffer}")
    }
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
            Self::LetStatement(s) => {
                buffer.push_str(&format!(
                    "{} {} = {};",
                    s.token.token_literal(),
                    s.name.token_literal(),
                    s.value
                ));
            }
            Self::ReturnStatement(s) => buffer.push_str(&s.to_string()),
            Self::ExpressStatement(s) => buffer.push_str(&s.to_string()),
        };

        write!(f, "{buffer}")
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::new();
        match self {
            Self::IntExpression(t) => buffer.push_str(&t.token_literal()),
            Self::InfixExpression(t) => buffer.push_str(&format!(
                "({} {} {})",
                t.1.value,
                t.0.token_literal(),
                t.2.value
            )),
            Self::IdentExpression(t) => buffer.push_str(&t.token_literal()),
            Self::PrefixExpression(t) => {
                buffer.push_str(&format!("({}{})", t.0.token_literal(), t.1.value))
            }
            Self::BoolExpression(t) => buffer.push_str(&t.token_literal()),
            Self::IfExpression(_, condition, consequnce, alternative) => {
                buffer.push_str(&format!(
                    "if {} {}",
                    condition.to_string(),
                    consequnce.to_string()
                ));

                if let Some(alt) = alternative {
                    buffer.push_str(&format!("else {}", alt.to_string()));
                }
            }
            Self::UnknownExpression(t) => buffer.push_str(&t.token_literal()),
        }

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
        write!(f, "{}", &self.value)
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

        assert_eq!(program.to_string(), "let test = fn;");
    }

    #[test]
    fn test_iden() {
        let mut program = Program::new();

        program
            .statements
            .push(Statement::LetStatement(LetStatement {
                token: Token::LET,
                name: Token::IDENT("myVar".to_string()),
                value: Expression::IdentExpression(Token::IDENT("anotherVar".to_string())),
            }));

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn test_prefix() {
        let mut program = Program::new();
        program
            .statements
            .push(Statement::ExpressStatement(ExpressionStatement {
                token: Token::BANG,
                value: Expression::PrefixExpression((
                    Token::BANG,
                    Box::new(ExpressionStatement {
                        token: Token::IDENT("a".to_string()),
                        value: Expression::PrefixExpression((
                            Token::MINUS,
                            Box::new(ExpressionStatement {
                                token: Token::IDENT("a".to_string()),
                                value: Expression::IntExpression(Token::IDENT("a".to_string())),
                            }),
                        )),
                    }),
                )),
            }));

        assert_eq!(program.to_string(), "(!(-a))");
    }
}
