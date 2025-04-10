use crate::{
    ast::{Expression, Map, TokenLiteral},
    object::ObjectType,
    parser::{ExpressionPrecendence, Parser},
};
use std::fmt::Display;
use std::{cell::LazyCell, collections::HashMap};

#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Clone, Default, Hash, Eq)]
pub enum Token {
    ILLEGAL(String),
    EOF,

    // Identifiers & literals
    IDENT(String),
    INT(usize),
    STRING(String),

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
    COLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

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

    if !p.expect_peek(&Token::RPAREN) {
        return None;
    }

    exp
}

fn parse_function_literal(p: &mut Parser) -> Option<Expression> {
    let token = p.cur_token.clone();

    if !p.expect_peek(&Token::LPAREN) {
        return None;
    }

    let parameters = p.parse_function_parameters()?;

    if !p.expect_peek(&Token::LBRACE) {
        return None;
    }

    let body = p.parse_block_statement().ok()?;

    Some(Expression::FunctionLiteral(token, parameters, body))
}

fn parse_if_expression(p: &mut Parser) -> Option<Expression> {
    if !p.expect_peek(&Token::LPAREN) {
        return None;
    }

    p.next_token();

    let condition = p.parse_expression(ExpressionPrecendence::LOWEST)?;

    if !p.expect_peek(&Token::RPAREN) {
        return None;
    }

    if !p.expect_peek(&Token::LBRACE) {
        return None;
    }

    let consequence = p.parse_block_statement().ok()?;
    let mut alternative = None;

    if p.peek_token_is(&Token::ELSE) {
        p.next_token();

        if !p.expect_peek(&Token::LBRACE) {
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
        p.parse_expression_list(&Token::RBRACKET)?,
    ))
}

fn parse_index_expression(p: &mut Parser, left: Expression) -> Expression {
    p.next_token();
    let index = p
        .parse_expression(ExpressionPrecendence::LOWEST)
        .unwrap_or_default();

    if !p.expect_peek(&Token::RBRACKET) {
        return Expression::UnknownExpression(Token::ILLEGAL("Failed to parse".to_string()));
    }

    Expression::IndexExpression(Box::new(left), Box::new(index))
}

fn parse_hash_literal(p: &mut Parser) -> Option<Expression> {
    let mut pairs = HashMap::new();

    while !p.peek_token_is(&Token::RBRACE) {
        p.next_token();
        let key = p.parse_expression(ExpressionPrecendence::LOWEST)?;

        if !p.expect_peek(&Token::COLON) {
            return None;
        }

        p.next_token();
        let value = p.parse_expression(ExpressionPrecendence::LOWEST)?;

        // this is a hacky way to get around hashing the Expression
        pairs.insert(key, value);

        if !p.peek_token_is(&Token::RBRACE) && !p.expect_peek(&Token::COMMA) {
            return None;
        }
    }

    // when would ever not pass?
    // this could probably just be p.next_token()
    if !p.expect_peek(&Token::RBRACE) {
        return None;
    }

    Some(Expression::HashLiteral(Map { pairs }))
}

impl Token {
    pub fn prefix_function(&self) -> Option<fn(&mut Parser) -> Option<Expression>> {
        match self {
            Token::STRING(_) => Some(parse_string),
            Token::IDENT(_) => Some(parse_ident),
            Token::INT(_) => Some(parse_int),
            Token::BANG | Token::MINUS => Some(parse_prefix_expression),
            Token::TRUE | Token::FALSE => Some(parse_bool_expression),
            Token::LPAREN => Some(parse_grouped_expression),
            Token::IF => Some(parse_if_expression),
            Token::FUNCTION => Some(parse_function_literal),
            Token::LBRACKET => Some(parse_array_expression),
            Token::LBRACE => Some(parse_hash_literal),
            _ => None,
        }
    }

    pub fn infix_function(&self) -> Option<fn(&mut Parser, Expression) -> Expression> {
        match self {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::EQ
            | Token::NOT_EQ
            | Token::LT
            | Token::GT => Some(parse_infix_expression),
            Token::LPAREN => Some(parse_call_expression),
            Token::LBRACKET => Some(parse_index_expression),
            _ => None,
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
            Token::STRING(v) => v.to_string(),
            Token::LBRACKET => "[".to_string(),
            Token::RBRACKET => "]".to_string(),
            Token::COLON => ":".to_string(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
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
