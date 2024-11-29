use crate::{
    ast::{Expression, ExpressionStatement, LetStatement, Program, ReturnStatement, Statement},
    lexer::Lexer,
    token::Token,
};
use std::mem;

enum ExpressionPrecendences {
    LOWEST = 1,
    EQUALS = 2,
    LessGreater = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    pub cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
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

    fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token: &Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        );
        self.errors.push(msg)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program {
            statements: Vec::default(),
        };

        while &self.cur_token != &Token::EOF {
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
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precendence: ExpressionPrecendences) -> Option<Expression> {
        self.cur_token.prefix_function().map(|f| f(self))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let mut statement = ExpressionStatement {
            token: self.cur_token.clone(),
            value: Expression::default(),
        };

        statement.value = self
            .parse_expression(ExpressionPrecendences::LOWEST)
            .ok_or("No expression found")?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::ExpressStatement(statement))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let statement = ReturnStatement {
            token: self.cur_token.clone(),
            value: Expression::default(),
        };

        self.next_token();

        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::ReturnStatement(statement))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let mut statement = LetStatement {
            token: self.cur_token.clone(),
            name: Token::default(),
            value: Expression::default(),
        };

        if !self.expect_peek(Token::IDENT(String::default())) {
            return Err("Failed".to_string());
        }

        statement.name = self.cur_token.clone();

        if !self.expect_peek(Token::ASSIGN) {
            return Err("Failed".to_string());
        }

        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::LetStatement(statement))
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        mem::discriminant(&self.peek_token) == mem::discriminant(token)
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(&token) {
            self.next_token();
            return true;
        }
        self.peek_error(&token);
        false
    }

    fn cur_token_is(&self, token: Token) -> bool {
        mem::discriminant(&self.cur_token) == mem::discriminant(&token)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{ExpressionStatement, Statement, TokenLiteral},
        token::Token,
    };
    use core::panic;

    struct ExpectedIdent {
        ident: &'static str,
    }

    fn test_statement(statement: &Statement, name: &str) -> bool {
        match statement {
            Statement::LetStatement(statement) => {
                if statement.token != Token::LET {
                    eprint!("Expected Let token, got: {:?}", statement.token);
                    return false;
                }
                if statement.name.token_literal() != name {
                    eprint!("Expected name {:?}, got: {:?}", name, statement.token);
                    return false;
                }
            }
            Statement::ReturnStatement(statement) => {
                if statement.token != Token::RETURN {
                    eprint!("Expected Let token, got: {:?}", statement.token);
                    return false;
                }
                // Test value here later
            }
            _ => todo!(),
        }

        true
    }

    fn check_parse_errors(p: &Parser) {
        let errors = p.errors();

        if errors.len() == 0 {
            return;
        }

        eprintln!("parser has {} errors", errors.len());
        for msg in errors.iter() {
            eprintln!("parser error: {}", msg);
        }

        panic!();
    }

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        let program = program.unwrap();

        if program.statements.len() != 3 {
            panic!("There are 3 statements!");
        }

        let test_idents = vec![
            ExpectedIdent { ident: "x" },
            ExpectedIdent { ident: "y" },
            ExpectedIdent { ident: "foobar" },
        ];

        for (i, expected) in test_idents.iter().enumerate() {
            let statement = program.statements.get(i).unwrap();
            if !test_statement(statement, expected.ident) {
                panic!("test failed!");
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"return 5;
        return 10;
        return 993322;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        let program = program.unwrap();

        if program.statements.len() != 3 {
            panic!("There are 3 statements!");
        }

        for statement in program.statements.iter() {
            if !test_statement(statement, "n/a") {
                panic!("test failed!");
            }
        }
    }

    #[test]
    fn test_indet_expression() {
        let input = "foobar";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(&parser);

        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!("There should only be one statement");
        }

        match &program.statements[0] {
            Statement::ExpressStatement(statement) => match &statement.value {
                Expression::IdentExpression(token) => {
                    assert_eq!(&token.token_literal(), "foobar")
                }
                _ => panic!("Expected IdentExpression"),
            },
            _ => panic!("There should only be an expression statement"),
        }
    }
}
