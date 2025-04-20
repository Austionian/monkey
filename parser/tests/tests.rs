use ast::*;
use core::panic;
use lexer::Lexer;
use parser::*;
use std::collections::HashMap;
use token::{Token, TokenLiteral};

fn test_statement(statement: &Statement, name: &str) -> bool {
    match statement {
        Statement::LetStatement(statement) => {
            if statement.token != Token::Let {
                eprint!("Expected Let token, got: {:?}", statement.token);
                return false;
            }
            if statement.name.token_literal() != name {
                eprint!("Expected name {:?}, got: {:?}", name, statement.token);
                return false;
            }
        }
        Statement::ReturnStatement(statement) => {
            if statement.token != Token::Return {
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
                    Token::Int(value) => assert_eq!(*value, 5),
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
                            Token::Int(value) => assert_eq!(value, &expected_int[i]),
                            _ => panic!("only ints here"),
                        },
                        Expression::BoolExpression(ref t) => match t {
                            Token::False => assert_eq!(t.token_literal(), expected_bool[i - 2]),
                            Token::True => assert_eq!(t.token_literal(), expected_bool[i - 2]),
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
                Expression::InfixExpression((infix_token, left_expression, right_expression)) => {
                    assert_eq!(&infix_token.token_literal(), expected_infix[i]);

                    match **left_expression {
                        Expression::IntExpression(ref t) => {
                            if let Token::Int(value) = t {
                                assert_eq!(value, &expected_int[i]);
                            }
                        }
                        _ => panic!("Only INT token expected"),
                    }

                    match **right_expression {
                        Expression::IntExpression(ref t) => {
                            if let Token::Int(value) = t {
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
                Expression::InfixExpression((infix_token, left_expression, right_expression)) => {
                    assert_eq!(&infix_token.token_literal(), expected_infix[i]);

                    if let Expression::BoolExpression(ref t) = **left_expression {
                        match t {
                            Token::False | Token::True => {
                                assert_eq!(t.token_literal(), expected_bool[i])
                            }
                            _ => panic!("Only bool token expected"),
                        }
                    } else {
                        panic!("Expected Bool expression")
                    }

                    if let Expression::BoolExpression(ref t) = **right_expression {
                        match t {
                            Token::False | Token::True => {
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
        ("3 > 5 || 5 < 3", "((3 > 5) || (5 < 3))"),
        ("3 == 5 || 5 == 5", "((3 == 5) || (5 == 5))"),
        ("3 > 5 && 5 < 3", "((3 > 5) && (5 < 3))"),
        ("3 == 5 && 5 == 5", "((3 == 5) && (5 == 5))"),
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
            Expression::FunctionLiteral(_, params, body, _) => {
                assert_eq!(params.len(), 2);
                if let Token::Ident(x) = &params[0] {
                    assert_eq!(x, "x");
                }
                if let Token::Ident(y) = &params[1] {
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
                Expression::FunctionLiteral(_, params, _, _) => {
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

#[test]
fn test_parsing_hash_literal() {
    let input = "{\"one\": 1, \"two\": 2, \"three\": 3};";
    let program = test_setup!(input);

    assert_eq!(program.statements.len(), 1);

    let mut expected = HashMap::new();

    expected.insert("one", 1);
    expected.insert("two", 2);
    expected.insert("three", 3);

    match &program.statements[0] {
        Statement::ExpressStatement(expression) => match &expression {
            Expression::HashLiteral(map) => {
                assert_eq!(map.pairs.len(), 3);
                for (k, v) in map.pairs.iter() {
                    let key = k.clone().to_owned();
                    let expected_value = expected.get(key.to_string().as_str()).unwrap();
                    test_int_expression(v, *expected_value);
                }
            }
            _ => panic!("expected hash literal"),
        },
        _ => panic!("expected expression statement"),
    }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{};";
    let program = test_setup!(input);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::ExpressStatement(expression) => match expression {
            Expression::HashLiteral(map) => assert_eq!(map.pairs.len(), 0),
            _ => panic!("expected hash literal"),
        },
        _ => panic!("expected expression statement"),
    }
}

#[test]
fn test_function_literal_with_name() {
    let input = "let myFunction = fn() { };";

    let program = test_setup!(input);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::LetStatement(statement) => match &statement.value {
            Expression::FunctionLiteral(_, _, _, name) => {
                assert_eq!(name.borrow().clone().unwrap(), "myFunction");
            }
            _ => panic!("expected function literal"),
        },
        _ => panic!("expected let statement"),
    }
}

#[test]
fn test_or() {
    let input = "true || false;";
    let program = test_setup!(input);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::ExpressStatement(expression) => match expression {
            Expression::InfixExpression((infix, _, _)) => {
                assert_eq!(*infix, Token::Or);
            }
            _ => panic!("expected infix expression"),
        },
        _ => panic!("expected expression statement"),
    }
}

#[test]
fn test_and() {
    let input = "true && false;";
    let program = test_setup!(input);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::ExpressStatement(expression) => match expression {
            Expression::InfixExpression((infix, _, _)) => {
                assert_eq!(*infix, Token::And);
            }
            _ => panic!("expected infix expression"),
        },
        _ => panic!("expected expression statement"),
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
