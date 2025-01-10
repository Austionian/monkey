use crate::{
    ast::{BlockStatement, Expression, LetStatement, Program, ReturnStatement, Statement},
    lexer::Lexer,
    token::Token,
};
use std::{cell::LazyCell, collections::HashMap, mem};

#[derive(Clone, PartialEq, PartialOrd)]
pub enum ExpressionPrecendence {
    LOWEST = 1,
    EQUALS = 2,
    LessGreater = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
    INDEX = 8,
}

const TOKEN_PRECEDENCES: LazyCell<HashMap<Token, ExpressionPrecendence>> = LazyCell::new(|| {
    let mut map = HashMap::new();

    map.insert(Token::EQ, ExpressionPrecendence::EQUALS);
    map.insert(Token::NOT_EQ, ExpressionPrecendence::EQUALS);
    map.insert(Token::LT, ExpressionPrecendence::LessGreater);
    map.insert(Token::GT, ExpressionPrecendence::LessGreater);
    map.insert(Token::PLUS, ExpressionPrecendence::SUM);
    map.insert(Token::MINUS, ExpressionPrecendence::SUM);
    map.insert(Token::SLASH, ExpressionPrecendence::PRODUCT);
    map.insert(Token::ASTERISK, ExpressionPrecendence::PRODUCT);
    map.insert(Token::LPAREN, ExpressionPrecendence::CALL);
    map.insert(Token::LBRACKET, ExpressionPrecendence::INDEX);

    map
});

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

        while self.cur_token != Token::EOF {
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

    pub fn parse_expression(&mut self, precendence: ExpressionPrecendence) -> Option<Expression> {
        let prefix_fn = self.cur_token.prefix_function();
        if prefix_fn.is_none() {
            self.no_prefix_parse_error();
            return None;
        }
        let prefix_fn = prefix_fn?;
        let mut left_expression = prefix_fn(self)?;

        while !self.peek_token_is(&Token::SEMICOLON) && precendence < self.peek_precedence() {
            let infix_fn = self.peek_token.infix_function();
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
            .parse_expression(ExpressionPrecendence::LOWEST)
            .ok_or("No expression found")?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::ExpressStatement(statement))
    }

    pub fn parse_block_statement(&mut self) -> Result<BlockStatement, String> {
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(Token::RBRACE) && !self.cur_token_is(Token::EOF) {
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
            .parse_expression(ExpressionPrecendence::LOWEST)
            .ok_or("Failed to parse expression")?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::ReturnStatement(statement))
    }

    pub fn parse_call_arguements(&mut self) -> Option<Vec<Expression>> {
        self.parse_expression_list(&Token::RPAREN)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let mut statement = LetStatement {
            token: self.cur_token.clone(),
            name: Token::default(),
            value: Expression::default(),
        };

        if !self.expect_peek(&Token::IDENT(String::default())) {
            return Err("Failed".to_string());
        }

        statement.name = self.cur_token.clone();

        if !self.expect_peek(&Token::ASSIGN) {
            return Err("Failed to parse let statement".to_string());
        }

        self.next_token();

        statement.value = self
            .parse_expression(ExpressionPrecendence::LOWEST)
            .ok_or("failed to parse expression")?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::LetStatement(statement))
    }

    pub fn parse_function_parameters(&mut self) -> Option<Vec<Token>> {
        let mut parameters = Vec::new();

        if self.peek_token_is(&Token::RPAREN) {
            self.next_token();
            return Some(parameters);
        }

        self.next_token();

        parameters.push(self.cur_token.clone());

        while self.peek_token_is(&Token::COMMA) {
            // advance to comma
            self.next_token();
            // advance to the ident
            self.next_token();

            parameters.push(self.cur_token.clone());
        }

        if !self.expect_peek(&Token::RPAREN) {
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
            ExpressionPrecendence::LOWEST
        }
    }

    pub fn cur_precendence(&self) -> ExpressionPrecendence {
        if let Some(precedence) = TOKEN_PRECEDENCES.get(&self.cur_token) {
            precedence.clone()
        } else {
            ExpressionPrecendence::LOWEST
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
        list.push(self.parse_expression(ExpressionPrecendence::LOWEST)?);

        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(ExpressionPrecendence::LOWEST)?);
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }
}

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{Statement, TokenLiteral},
        test_setup,
        token::Token,
    };
    use core::panic;

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

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 3);

        let test_idents = vec!["x", "y", "foobar"];

        for (i, expected) in test_idents.iter().enumerate() {
            let statement = program.statements.get(i).unwrap();
            assert!(test_statement(statement, expected))
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"return 5;
        return 10;
        return 993322;
        "#;

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 3);

        for statement in program.statements.iter() {
            assert!(test_statement(statement, "n/a"))
        }
    }

    #[test]
    fn test_indet_expression() {
        let input = "foobar";

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(statement) => match &statement {
                Expression::IdentExpression(token) => {
                    assert_eq!(&token.token_literal(), "foobar")
                }
                _ => panic!("Expected IdentExpression"),
            },
            _ => panic!("There should only be an expression statement"),
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(statement) => match &statement {
                Expression::IntExpression(token) => {
                    assert_eq!(&token.token_literal(), "5");

                    match token {
                        Token::INT(value) => assert_eq!(*value, 5),
                        _ => panic!("Only INT token expected."),
                    }
                }
                _ => panic!("Expected IntExpression"),
            },
            _ => panic!("There should only be an expression statement"),
        }
    }

    #[test]
    fn test_bool_expression() {
        let inputs = ["false;", "true;"];
        let expected = ["false", "true"];

        for (i, input) in inputs.iter().enumerate() {
            let program = test_setup!(input);

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::ExpressStatement(statement) => match &statement {
                    Expression::BoolExpression(token) => {
                        assert_eq!(&token.token_literal(), expected[i]);
                    }
                    _ => panic!("Expected BoolExpression"),
                },
                _ => panic!("There should only be an expression statement"),
            }
        }
    }

    #[test]
    fn test_prefix_operator() {
        let inputs = vec!["!5;", "-15", "!false", "!true"];
        let expected_prefix = vec!["!", "-", "!", "!"];
        let expected_int = vec![5, 15];
        let expected_bool = vec!["false", "true"];

        inputs.iter().enumerate().for_each(|(i, input)| {
            let program = test_setup!(input);

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::ExpressStatement(statement) => match &statement {
                    Expression::PrefixExpression((prefix_token, int_expression)) => {
                        assert_eq!(&prefix_token.token_literal(), expected_prefix[i]);

                        match **int_expression {
                            Expression::IntExpression(ref t) => match t {
                                Token::INT(value) => assert_eq!(value, &expected_int[i]),
                                _ => panic!("only ints here"),
                            },
                            Expression::BoolExpression(ref t) => match t {
                                Token::FALSE => assert_eq!(t.token_literal(), expected_bool[i - 2]),
                                Token::TRUE => assert_eq!(t.token_literal(), expected_bool[i - 2]),
                                _ => panic!("Only Bool token expected."),
                            },
                            _ => panic!("Only INT"),
                        }
                    }
                    _ => panic!("Expected PrefixExpression"),
                },
                _ => panic!("There should only be an expression statement"),
            }
        })
    }

    #[test]
    fn test_infix_operators() {
        let inputs = vec![
            "5 + 5", "5 - 5", "5 * 5", "5 / 5", "5 > 5", "5 < 5", "5 == 5", "5 != 5",
        ];
        let expected_infix = vec!["+", "-", "*", "/", ">", "<", "==", "!="];
        let expected_int = vec![5; 8];

        inputs.iter().enumerate().for_each(|(i, input)| {
            let program = test_setup!(input);

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::ExpressStatement(statement) => match &statement {
                    Expression::InfixExpression((
                        infix_token,
                        left_expression,
                        right_expression,
                    )) => {
                        assert_eq!(&infix_token.token_literal(), expected_infix[i]);

                        match **left_expression {
                            Expression::IntExpression(ref t) => {
                                if let Token::INT(value) = t {
                                    assert_eq!(value, &expected_int[i]);
                                }
                            }
                            _ => panic!("Only INT token expected"),
                        }

                        match **right_expression {
                            Expression::IntExpression(ref t) => {
                                if let Token::INT(value) = t {
                                    assert_eq!(value, &expected_int[i]);
                                }
                            }
                            _ => panic!("Only INT token expected"),
                        }
                    }
                    _ => panic!("Expected InfixExpression"),
                },
                _ => panic!("There should only be an ExpressionStatement"),
            }
        });

        let inputs = vec!["true == true", "true != false", "true == true"];
        let expected_infix = vec!["==", "!=", "=="];
        // first three are expected left, next three are expected right
        let expected_bool = vec!["true", "true", "true", "true", "false", "true"];

        inputs.iter().enumerate().for_each(|(i, input)| {
            let program = test_setup!(input);

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::ExpressStatement(statement) => match &statement {
                    Expression::InfixExpression((
                        infix_token,
                        left_expression,
                        right_expression,
                    )) => {
                        assert_eq!(&infix_token.token_literal(), expected_infix[i]);

                        if let Expression::BoolExpression(ref t) = **left_expression {
                            match t {
                                Token::FALSE | Token::TRUE => {
                                    assert_eq!(t.token_literal(), expected_bool[i])
                                }
                                _ => panic!("Only bool token expected"),
                            }
                        } else {
                            panic!("Expected Bool expression")
                        }

                        if let Expression::BoolExpression(ref t) = **right_expression {
                            match t {
                                Token::FALSE | Token::TRUE => {
                                    assert_eq!(t.token_literal(), expected_bool[i + 3])
                                }
                                _ => panic!("Only bool token expected"),
                            }
                        } else {
                            panic!("Expected Bool expression")
                        }
                    }
                    _ => panic!("Expected InfixExpression"),
                },
                _ => panic!("There should only be an ExpressionStatement"),
            }
        });
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == false", "((3 < 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        inputs.iter().enumerate().for_each(|(i, input)| {
            let program = test_setup!(input.0);

            assert_eq!(program.to_string(), inputs[i].1);
        });
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(expression_statement) => match &expression_statement {
                Expression::IfExpression(conditional, consequence, alt) => {
                    test_infix_expression(&conditional, "<", "x", "y");

                    match &consequence.statements[0] {
                        Statement::ExpressStatement(exp) => match &exp {
                            Expression::IdentExpression(t) => {
                                assert_eq!(t.token_literal(), "x");
                            }
                            _ => panic!("Expected IdentExpression"),
                        },
                        _ => panic!("Expected an ExpressionStatement"),
                    }

                    assert!(alt.is_none());
                }
                _ => panic!("Should only be an if expression"),
            },
            _ => panic!("Should first be an expression statement"),
        }
    }

    #[test]
    fn test_if_expression_with_else() {
        let input = "if (x < y) { x } else { y }";

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(expression_statement) => match &expression_statement {
                Expression::IfExpression(conditional, consequence, alt) => {
                    test_infix_expression(&conditional, "<", "x", "y");
                    match **conditional {
                        Expression::InfixExpression((ref t, ref left, ref right)) => {
                            if let Expression::IdentExpression(ref t) = **left {
                                assert_eq!(t.token_literal(), "x");
                            }
                            assert_eq!(t.token_literal(), "<");
                            if let Expression::IdentExpression(ref t) = **right {
                                assert_eq!(t.token_literal(), "y");
                            }
                        }
                        _ => panic!("Expected an infix expression"),
                    }

                    match &consequence.statements[0] {
                        Statement::ExpressStatement(exp) => match &exp {
                            Expression::IdentExpression(t) => {
                                assert_eq!(t.token_literal(), "x");
                            }
                            _ => panic!("Expected IdentExpression"),
                        },
                        _ => panic!("Expected an ExpressionStatement"),
                    }

                    if let Some(alternative) = alt {
                        match &alternative.statements[0] {
                            Statement::ExpressStatement(exp) => match &exp {
                                Expression::IdentExpression(t) => {
                                    assert_eq!(t.token_literal(), "y");
                                }
                                _ => panic!("Expected IdentExpression"),
                            },
                            _ => panic!("Expected an ExpressionStatement"),
                        }
                    }
                }
                _ => panic!("Should only be an if expression"),
            },
            _ => panic!("Should first be an expression statement"),
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(expression) => match &expression {
                Expression::FunctionLiteral(_, params, body) => {
                    assert_eq!(params.len(), 2);
                    if let Token::IDENT(x) = &params[0] {
                        assert_eq!(x, "x");
                    }
                    if let Token::IDENT(y) = &params[1] {
                        assert_eq!(y, "y");
                    }

                    assert_eq!(body.statements.len(), 1);
                    match &body.statements[0] {
                        Statement::ExpressStatement(exp) => {
                            test_infix_expression(&exp, "+", "x", "y");
                        }
                        _ => panic!("Expected expression statement"),
                    }
                }
                _ => panic!("Expected function literal"),
            },
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        let inputs = vec!["fn() {};", "fn(x) {};", "fn(x, y, z) {};"];
        let expected = vec![vec![], vec!["x"], vec!["x", "y", "z"]];

        for (i, input) in inputs.iter().enumerate() {
            let program = test_setup!(input);

            match &program.statements[0] {
                Statement::ExpressStatement(expression) => match &expression {
                    Expression::FunctionLiteral(_, params, _) => {
                        assert_eq!(params.len(), expected[i].len());
                        for (j, param) in params.iter().enumerate() {
                            assert_eq!(param.token_literal(), expected[i][j])
                        }
                    }
                    _ => panic!("Expected function literal"),
                },
                _ => panic!("Expected expression"),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";

        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(expression) => match &expression {
                Expression::CallExpression(func, args) => {
                    test_ident_expression(&func, "add");
                    assert_eq!(args.len(), 3);
                    test_int_expression(&args[0], 1);
                    test_infix_expression(&args[1], "*", "2", "3");
                    test_infix_expression(&args[2], "+", "4", "5");
                }
                _ => panic!("Expected call expression"),
            },
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\"";
        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(expression) => match &expression {
                Expression::StringExpression(t) => {
                    assert_eq!(t.token_literal(), "hello world")
                }
                _ => panic!("Expected string expression"),
            },
            _ => panic!("Exprected a expression statement"),
        }
    }

    #[test]
    fn test_parsing_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";
        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(expression) => match &expression {
                Expression::ArrayExpression(array) => {
                    assert_eq!(array.len(), 3);
                    test_int_expression(&array[0], 1);
                    test_infix_expression(&array[1], "*", "2", "2");
                    test_infix_expression(&array[2], "+", "3", "3");
                }
                _ => unreachable!("Expected an array expression"),
            },
            _ => unreachable!("Expected an expression statement"),
        }
    }

    #[test]
    fn test_parsing_index_expression() {
        let input = "myArray[1 + 1];";
        let program = test_setup!(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressStatement(expression) => match &expression {
                Expression::IndexExpression(left, index) => {
                    test_ident_expression(&left, "myArray");
                    test_infix_expression(&index, "+", "1", "1");
                }
                _ => unreachable!("Expected an index expression, got {expression:?}"),
            },
            _ => unreachable!("Expreced an expression statement"),
        }
    }

    fn test_ident_expression(expression: &Expression, expected_token: &str) {
        match expression {
            Expression::IdentExpression(t) => {
                assert_eq!(t.token_literal(), expected_token);
            }
            _ => panic!("Expected ident expression"),
        }
    }

    fn test_int_expression(expression: &Expression, expected_token: usize) {
        match expression {
            Expression::IntExpression(t) => {
                assert_eq!(t.token_literal().parse::<usize>().unwrap(), expected_token);
            }
            _ => panic!("Expected int expression"),
        }
    }

    fn test_infix_expression(
        expression: &Expression,
        expected_token: &str,
        left_literal: &str,
        right_literal: &str,
    ) {
        match expression {
            Expression::InfixExpression((t, left, right)) => {
                assert_eq!(t.token_literal(), expected_token);
                match **left {
                    Expression::IdentExpression(ref t) => {
                        assert_eq!(t.token_literal(), left_literal)
                    }
                    Expression::IntExpression(ref t) => {
                        assert_eq!(t.token_literal(), left_literal)
                    }
                    _ => panic!("Expected ident | int expression"),
                }

                match **right {
                    Expression::IdentExpression(ref t) => {
                        assert_eq!(t.token_literal(), right_literal)
                    }
                    Expression::IntExpression(ref t) => {
                        assert_eq!(t.token_literal(), right_literal)
                    }
                    _ => panic!("Expected ident | int expression"),
                }
            }
            _ => panic!("Expected infix expression"),
        }
    }
}

#[macro_export]
macro_rules! test_setup {
    ($input: expr) => {{
        let lexer = Lexer::new($input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(&parser);

        program.unwrap()
    }};
}
