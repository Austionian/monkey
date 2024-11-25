use std::{cell::LazyCell, collections::HashMap};

#[derive(PartialEq, Debug)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers & literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    PLUS,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
}

const KEYWORDS: LazyCell<HashMap<&'static str, TokenType>> = LazyCell::new(|| {
    let mut map = HashMap::new();
    map.insert("fn", TokenType::FUNCTION);
    map.insert("let", TokenType::LET);

    map
});

pub fn look_up_ident(ident: &str) -> TokenType {
    if let Some((_, token)) = KEYWORDS.get_key_value(ident) {
        match token {
            TokenType::FUNCTION => TokenType::FUNCTION,
            TokenType::LET => TokenType::LET,
            _ => unreachable!("No other keywords"),
        }
    } else {
        TokenType::IDENT
    }
}

pub struct Token {
    pub r#type: TokenType,
    pub literal: &'static str,
}
