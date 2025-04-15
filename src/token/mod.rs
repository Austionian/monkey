use crate::{
    ast::{Expression, Map, TokenLiteral},
    parser::{ExpressionPrecendence, Parser},
};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc, sync::LazyLock};

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

    Lt,
    Gt,

    Eq,
    Not_eq,

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

fn parse_ident(p: &mut Parser) -> Option<Expression> {
    Some(Expression::IdentExpression(p.cur_token.clone()))
}

fn parse_int(p: &mut Parser) -> Option<Expression> {
    Some(Expression::IntExpression(p.cur_token.clone()))
}

fn parse_prefix_expression(p: &mut Parser) -> Option<Expression> {
    let prefix = p.cur_token.clone();
    p.next_token();
    let right = p.parse_expression(ExpressionPrecendence::PREFIX).unwrap();
    Some(Expression::PrefixExpression((prefix, Box::new(right))))
}

fn parse_bool_expression(p: &mut Parser) -> Option<Expression> {
    Some(Expression::BoolExpression(p.cur_token.clone()))
}

fn parse_grouped_expression(p: &mut Parser) -> Option<Expression> {
    p.next_token();

    let exp = p.parse_expression(ExpressionPrecendence::LOWEST);

    if !p.expect_peek(&Token::Rparen) {
        return None;
    }

    exp
}

fn parse_function_literal(p: &mut Parser) -> Option<Expression> {
    let token = p.cur_token.clone();

    if !p.expect_peek(&Token::Lparen) {
        return None;
    }

    let parameters = p.parse_function_parameters()?;

    if !p.expect_peek(&Token::Lbrace) {
        return None;
    }

    let body = p.parse_block_statement().ok()?;

    Some(Expression::FunctionLiteral(
        token,
        parameters,
        body,
        Rc::new(RefCell::new(None)),
    ))
}

fn parse_if_expression(p: &mut Parser) -> Option<Expression> {
    if !p.expect_peek(&Token::Lparen) {
        return None;
    }

    p.next_token();

    let condition = p.parse_expression(ExpressionPrecendence::LOWEST)?;

    if !p.expect_peek(&Token::Rparen) {
        return None;
    }

    if !p.expect_peek(&Token::Lbrace) {
        return None;
    }

    let consequence = p.parse_block_statement().ok()?;
    let mut alternative = None;

    if p.peek_token_is(&Token::Else) {
        p.next_token();

        if !p.expect_peek(&Token::Lbrace) {
            return None;
        }

        alternative = Some(Box::new(p.parse_block_statement().ok()?));
    }

    Some(Expression::IfExpression(
        Box::new(condition),
        Box::new(consequence),
        alternative,
    ))
}

fn parse_call_expression(p: &mut Parser, function: Expression) -> Expression {
    // TODO: should maybe be handled as an error instead.
    let args = p.parse_call_arguments().unwrap_or_default();

    Expression::CallExpression(Box::new(function), args)
}

fn parse_infix_expression(p: &mut Parser, left: Expression) -> Expression {
    let infix = p.cur_token.clone();
    let precendence = p.cur_precendence();

    p.next_token();

    let right = p.parse_expression(precendence).unwrap_or_default();

    Expression::InfixExpression((infix, Box::new(left), Box::new(right)))
}

fn parse_string(p: &mut Parser) -> Option<Expression> {
    Some(Expression::StringExpression(p.cur_token.clone()))
}

fn parse_array_expression(p: &mut Parser) -> Option<Expression> {
    Some(Expression::ArrayExpression(
        p.parse_expression_list(&Token::Rbracket)?,
    ))
}

fn parse_index_expression(p: &mut Parser, left: Expression) -> Expression {
    p.next_token();
    let index = p
        .parse_expression(ExpressionPrecendence::LOWEST)
        .unwrap_or_default();

    if !p.expect_peek(&Token::Rbracket) {
        return Expression::UnknownExpression(Token::Illegal("Failed to parse".to_string()));
    }

    Expression::IndexExpression(Box::new(left), Box::new(index))
}

fn parse_hash_literal(p: &mut Parser) -> Option<Expression> {
    let mut pairs = HashMap::new();

    while !p.peek_token_is(&Token::Rbrace) {
        p.next_token();
        let key = p.parse_expression(ExpressionPrecendence::LOWEST)?;

        if !p.expect_peek(&Token::Colon) {
            return None;
        }

        p.next_token();
        let value = p.parse_expression(ExpressionPrecendence::LOWEST)?;

        // this is a hacky way to get around hashing the Expression
        pairs.insert(key, value);

        if !p.peek_token_is(&Token::Rbrace) && !p.expect_peek(&Token::Comma) {
            return None;
        }
    }

    // when would ever not pass?
    // this could probably just be p.next_token()
    if !p.expect_peek(&Token::Rbrace) {
        return None;
    }

    Some(Expression::HashLiteral(Map { pairs }))
}

impl Token {
    pub fn prefix_function(&self) -> Option<fn(&mut Parser) -> Option<Expression>> {
        match self {
            Token::String(_) => Some(parse_string),
            Token::Ident(_) => Some(parse_ident),
            Token::Int(_) => Some(parse_int),
            Token::Bang | Token::Minus => Some(parse_prefix_expression),
            Token::True | Token::False => Some(parse_bool_expression),
            Token::Lparen => Some(parse_grouped_expression),
            Token::If => Some(parse_if_expression),
            Token::Function => Some(parse_function_literal),
            Token::Lbracket => Some(parse_array_expression),
            Token::Lbrace => Some(parse_hash_literal),
            _ => None,
        }
    }

    pub fn infix_function(&self) -> Option<fn(&mut Parser, Expression) -> Expression> {
        match self {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Eq
            | Token::Not_eq
            | Token::Lt
            | Token::Gt => Some(parse_infix_expression),
            Token::Lparen => Some(parse_call_expression),
            Token::Lbracket => Some(parse_index_expression),
            _ => None,
        }
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
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
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
