use crate::{
    ast::{Expression, TokenLiteral},
    parser::Parser,
};
use std::{cell::LazyCell, collections::HashMap, process::Output};

#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Clone, Default)]
pub enum Token {
    ILLEGAL(String),
    EOF,

    // Identifiers & literals
    IDENT(String),
    INT(usize),

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NOT_EQ,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    #[default]
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

fn parse_ident(p: &mut Parser) -> Expression {
    Expression::IdentExpression(p.cur_token.clone())
}

impl Token {
    pub fn prefix_function(&self) -> Option<impl Fn(&mut Parser) -> Expression> {
        match self {
            Token::IDENT(_) => Some(parse_ident),
            _ => todo!(),
        }
    }
}

impl TokenLiteral for Token {
    fn token_literal(&self) -> String {
        match self {
            Token::ILLEGAL(v) => v.to_string(),
            Token::EOF => "\n".to_string(),
            Token::IDENT(v) => v.to_string(),
            Token::INT(v) => v.to_string(),
            Token::ASSIGN => "=".to_string(),
            Token::PLUS => "+".to_string(),
            Token::MINUS => "-".to_string(),
            Token::BANG => "!".to_string(),
            Token::ASTERISK => "*".to_string(),
            Token::SLASH => "/".to_string(),
            Token::LT => "<".to_string(),
            Token::GT => ">".to_string(),
            Token::EQ => "==".to_string(),
            Token::NOT_EQ => "!=".to_string(),
            Token::COMMA => ",".to_string(),
            Token::SEMICOLON => ";".to_string(),
            Token::LPAREN => "(".to_string(),
            Token::RPAREN => ")".to_string(),
            Token::LBRACE => "{".to_string(),
            Token::RBRACE => "}".to_string(),
            Token::FUNCTION => "fn".to_string(),
            Token::LET => "let".to_string(),
            Token::TRUE => "true".to_string(),
            Token::FALSE => "false".to_string(),
            Token::IF => "if".to_string(),
            Token::ELSE => "else".to_string(),
            Token::RETURN => "return".to_string(),
        }
    }
}

const KEYWORDS: LazyCell<HashMap<&'static str, Token>> = LazyCell::new(|| {
    let mut map = HashMap::new();
    map.insert("fn", Token::FUNCTION);
    map.insert("let", Token::LET);
    map.insert("true", Token::TRUE);
    map.insert("false", Token::FALSE);
    map.insert("if", Token::IF);
    map.insert("else", Token::ELSE);
    map.insert("return", Token::RETURN);

    map
});

pub fn look_up_ident(ident: &str) -> Token {
    if let Some((_, token)) = KEYWORDS.get_key_value(ident) {
        token.clone()
    } else {
        Token::IDENT(String::default())
    }
}
