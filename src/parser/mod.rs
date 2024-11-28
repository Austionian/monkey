use crate::{
    ast::{Expression, LetStatement, Program, Statement},
    lexer::Lexer,
    token::Token,
};
use std::mem;

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
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

    fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program {
            statements: Vec::default(),
        };

        while &self.cur_token != &Token::EOF {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }

        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            _ => todo!(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let mut statement = LetStatement {
            token: self.cur_token.clone(),
            name: Token::default(),
            value: Expression::default(),
        };

        if !self.expect_peek(Token::IDENT(String::default())) {
            return None;
        }

        statement.name = self.cur_token.clone();

        if !self.expect_peek(Token::ASSIGN) {
            return None;
        }

        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::LetStatement(statement))
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
        ast::{Statement, TokenLiteral},
        token::Token,
    };

    struct ExpectedIdent {
        ident: &'static str,
    }

    fn test_let_statement(statement: &Statement, name: &str) -> bool {
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

        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

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
            if !test_let_statement(statement, expected.ident) {
                panic!("test failed!");
            }
        }
    }
}
