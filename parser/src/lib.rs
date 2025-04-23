use ast::{BlockStatement, Expression, LetStatement, Map, Program, ReturnStatement, Statement};
use lexer::Lexer;
use std::{cell::RefCell, collections::HashMap, mem, rc::Rc, sync::LazyLock};
use token::Token;

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum ExpressionPrecendence {
    Lowest = 1,
    LogicalOperator = 2,
    Equals = 3,
    LessGreater = 4,
    Sum = 5,
    Product = 6,
    Prefix = 7,
    Call = 8,
    Index = 9,
}

static TOKEN_PRECEDENCES: LazyLock<HashMap<Token, ExpressionPrecendence>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert(Token::Or, ExpressionPrecendence::LogicalOperator);
    map.insert(Token::And, ExpressionPrecendence::LogicalOperator);
    map.insert(Token::Eq, ExpressionPrecendence::Equals);
    map.insert(Token::Not_eq, ExpressionPrecendence::Equals);
    map.insert(Token::Lt, ExpressionPrecendence::LessGreater);
    map.insert(Token::Gt, ExpressionPrecendence::LessGreater);
    map.insert(Token::Plus, ExpressionPrecendence::Sum);
    map.insert(Token::Minus, ExpressionPrecendence::Sum);
    map.insert(Token::Slash, ExpressionPrecendence::Product);
    map.insert(Token::Asterisk, ExpressionPrecendence::Product);
    map.insert(Token::Lparen, ExpressionPrecendence::Call);
    map.insert(Token::Lbracket, ExpressionPrecendence::Index);

    map
});

/// The Monkey Parser.
///
/// Parses Tokens into an AST.
#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    pub cur_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: Vec::default(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    #[cfg(test)]
    fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn no_prefix_parse_error(&mut self) {
        let msg = format!("no prefix parse function for {:?} found", self.cur_token);
        self.errors.push(msg)
    }

    fn peek_error(&mut self, token: &Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        );
        self.errors.push(msg)
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program {
            statements: Vec::default(),
        };

        while self.cur_token != Token::Eof {
            let statement = self.parse_statement();
            match statement {
                Ok(statement) => program.statements.push(statement),
                Err(e) => return Err(e),
            }
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_expression(&mut self, precendence: ExpressionPrecendence) -> Option<Expression> {
        let prefix_fn = prefix_function(&self.cur_token);
        if prefix_fn.is_none() {
            self.no_prefix_parse_error();
            return None;
        }
        let prefix_fn = prefix_fn?;
        let mut left_expression = prefix_fn(self)?;

        while !self.peek_token_is(&Token::Semicolon) && precendence < self.peek_precedence() {
            let infix_fn = infix_function(&self.peek_token);
            if infix_fn.is_none() {
                return Some(left_expression);
            }

            self.next_token();

            let infix_fn = infix_fn.unwrap();
            left_expression = infix_fn(self, left_expression);
        }

        Some(left_expression)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let statement = self
            .parse_expression(ExpressionPrecendence::Lowest)
            .ok_or("No expression found")?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::ExpressStatement(statement))
    }

    pub fn parse_block_statement(&mut self) -> Result<BlockStatement, String> {
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(Token::Rbrace) && !self.cur_token_is(Token::Eof) {
            let statement = self.parse_statement()?;
            statements.push(statement);

            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let mut statement = ReturnStatement {
            token: self.cur_token.clone(),
            value: Expression::default(),
        };

        self.next_token();

        statement.value = self
            .parse_expression(ExpressionPrecendence::Lowest)
            .ok_or("Failed to parse expression")?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::ReturnStatement(statement))
    }

    pub fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        self.parse_expression_list(&Token::Rparen)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let mut statement = LetStatement {
            token: self.cur_token.clone(),
            name: Token::default(),
            value: Expression::default(),
        };

        if !self.expect_peek(&Token::Ident(String::default())) {
            return Err("Failed".to_string());
        }

        statement.name = self.cur_token.clone();

        if !self.expect_peek(&Token::Assign) {
            return Err("Failed to parse let statement".to_string());
        }

        self.next_token();

        statement.value = self
            .parse_expression(ExpressionPrecendence::Lowest)
            .ok_or("failed to parse expression")?;

        if let Expression::FunctionLiteral(_, _, _, ref rc) = statement.value {
            let mut name = rc.borrow_mut();
            *name = Some(statement.name.to_string());
        }

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::LetStatement(statement))
    }

    pub fn parse_function_parameters(&mut self) -> Option<Vec<Token>> {
        let mut parameters = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Some(parameters);
        }

        self.next_token();

        parameters.push(self.cur_token.clone());

        while self.peek_token_is(&Token::Comma) {
            // advance to comma
            self.next_token();
            // advance to the ident
            self.next_token();

            parameters.push(self.cur_token.clone());
        }

        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        Some(parameters)
    }

    pub fn peek_token_is(&self, token: &Token) -> bool {
        mem::discriminant(&self.peek_token) == mem::discriminant(token)
    }

    pub fn peek_precedence(&self) -> ExpressionPrecendence {
        if let Some(precedence) = TOKEN_PRECEDENCES.get(&self.peek_token) {
            precedence.clone()
        } else {
            ExpressionPrecendence::Lowest
        }
    }

    pub fn cur_precendence(&self) -> ExpressionPrecendence {
        if let Some(precedence) = TOKEN_PRECEDENCES.get(&self.cur_token) {
            precedence.clone()
        } else {
            ExpressionPrecendence::Lowest
        }
    }

    pub fn expect_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            return true;
        }
        self.peek_error(token);
        false
    }

    fn cur_token_is(&self, token: Token) -> bool {
        mem::discriminant(&self.cur_token) == mem::discriminant(&token)
    }

    pub fn parse_expression_list(&mut self, end: &Token) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(self.parse_expression(ExpressionPrecendence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(ExpressionPrecendence::Lowest)?);
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }
}

#[cfg(test)]
pub fn check_parse_errors(p: &Parser) {
    let errors = p.errors();

    if errors.is_empty() {
        return;
    }

    eprintln!("parser has {} errors", errors.len());
    for msg in errors.iter() {
        eprintln!("parser error: {}", msg);
    }

    panic!();
}

#[macro_export]
macro_rules! test_setup {
    ($input: expr) => {{
        let lexer = Lexer::new($input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        program.unwrap()
    }};
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
    let right = p.parse_expression(ExpressionPrecendence::Prefix).unwrap();
    Some(Expression::PrefixExpression((prefix, Box::new(right))))
}

fn parse_bool_expression(p: &mut Parser) -> Option<Expression> {
    Some(Expression::BoolExpression(p.cur_token.clone()))
}

fn parse_grouped_expression(p: &mut Parser) -> Option<Expression> {
    p.next_token();

    let exp = p.parse_expression(ExpressionPrecendence::Lowest);

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

    let condition = p.parse_expression(ExpressionPrecendence::Lowest)?;

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
        .parse_expression(ExpressionPrecendence::Lowest)
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
        let key = p.parse_expression(ExpressionPrecendence::Lowest)?;

        if !p.expect_peek(&Token::Colon) {
            return None;
        }

        p.next_token();
        let value = p.parse_expression(ExpressionPrecendence::Lowest)?;

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

pub fn prefix_function(token: &Token) -> Option<fn(&mut Parser) -> Option<Expression>> {
    match token {
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

pub fn infix_function(token: &Token) -> Option<fn(&mut Parser, Expression) -> Expression> {
    match token {
        Token::Plus
        | Token::Minus
        | Token::Slash
        | Token::Asterisk
        | Token::Eq
        | Token::Not_eq
        | Token::Lt
        | Token::Gt
        | Token::Or
        | Token::And => Some(parse_infix_expression),
        Token::Lparen => Some(parse_call_expression),
        Token::Lbracket => Some(parse_index_expression),
        _ => None,
    }
}
