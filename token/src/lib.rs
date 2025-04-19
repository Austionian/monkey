use std::{collections::HashMap, fmt::Display, sync::LazyLock};

#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Clone, Default, Hash, Eq)]
pub enum Token {
    Illegal(String),
    Eof,

    // Identifiers & literals
    Ident(String),
    Int(usize),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    // Comparisons
    Lt,
    Gt,
    Eq,
    Not_eq,
    Or,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keywords
    #[default]
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub trait TokenLiteral {
    fn token_literal(&self) -> String;
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

impl TokenLiteral for Token {
    fn token_literal(&self) -> String {
        match self {
            Token::Illegal(v) => v.to_string(),
            Token::Eof => "\n".to_string(),
            Token::Ident(v) => v.to_string(),
            Token::Int(v) => v.to_string(),
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Bang => "!".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::Lt => "<".to_string(),
            Token::Gt => ">".to_string(),
            Token::Eq => "==".to_string(),
            Token::Not_eq => "!=".to_string(),
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Lparen => "(".to_string(),
            Token::Rparen => ")".to_string(),
            Token::Lbrace => "{".to_string(),
            Token::Rbrace => "}".to_string(),
            Token::Function => "fn".to_string(),
            Token::Let => "let".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
            Token::String(v) => v.to_string(),
            Token::Lbracket => "[".to_string(),
            Token::Rbracket => "]".to_string(),
            Token::Colon => ":".to_string(),
            Token::Or => "||".to_string(),
        }
    }
}

static KEYWORDS: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    let mut map = HashMap::new();
    map.insert("fn", Token::Function);
    map.insert("let", Token::Let);
    map.insert("true", Token::True);
    map.insert("false", Token::False);
    map.insert("if", Token::If);
    map.insert("else", Token::Else);
    map.insert("return", Token::Return);

    map
});

pub fn look_up_ident(ident: &str) -> Token {
    if let Some((_, token)) = KEYWORDS.get_key_value(ident) {
        token.clone()
    } else {
        Token::Ident(String::default())
    }
}
