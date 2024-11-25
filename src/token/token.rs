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

    LPARAN,
    RPARAN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
}

pub struct Token {
    pub r#type: TokenType,
    pub literal: &'static str,
}
