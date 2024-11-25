use std::{cell::LazyCell, collections::HashMap};

#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Clone, Default)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers & literals
    IDENT,
    INT,

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

const KEYWORDS: LazyCell<HashMap<&'static str, TokenType>> = LazyCell::new(|| {
    let mut map = HashMap::new();
    map.insert("fn", TokenType::FUNCTION);
    map.insert("let", TokenType::LET);
    map.insert("true", TokenType::TRUE);
    map.insert("false", TokenType::FALSE);
    map.insert("if", TokenType::IF);
    map.insert("else", TokenType::ELSE);
    map.insert("return", TokenType::RETURN);

    map
});

pub fn look_up_ident(ident: &str) -> TokenType {
    if let Some((_, token)) = KEYWORDS.get_key_value(ident) {
        token.clone()
    } else {
        TokenType::IDENT
    }
}

#[derive(Debug, Default)]
pub struct Token<'a> {
    pub r#type: TokenType,
    pub literal: &'a str,
}
