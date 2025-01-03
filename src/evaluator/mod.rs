use crate::{
    ast::{BlockStatement, Expression, Program, Statement},
    object::{Object, ObjectType},
    token::Token,
};

const TRUE: ObjectType = ObjectType::BoolObj(true);
const FALSE: ObjectType = ObjectType::BoolObj(false);
const NULL: ObjectType = ObjectType::NullObj;

pub fn eval_program(program: &Program) -> ObjectType {
    eval_statements(&program.statements)
}

pub fn eval_statements(statements: &Vec<Statement>) -> ObjectType {
    let mut result = ObjectType::default();

    for statement in statements {
        result = eval(&statement);

        if let ObjectType::ReturnValueObj(value) = result {
            return *value;
        }
        if let ObjectType::ErrorObj(_) = result {
            return result;
        }
    }

    result
}

pub fn eval(node: &Statement) -> ObjectType {
    match node {
        Statement::ExpressStatement(expression) => match &expression {
            Expression::IntExpression(int) => match int {
                Token::INT(v) => ObjectType::IntegerObj(*v as f64),
                _ => unreachable!("Only ints belong in int expressions"),
            },
            Expression::BoolExpression(bool) => match bool {
                Token::TRUE => TRUE,
                Token::FALSE => FALSE,
                _ => unreachable!("Only bools belong in bool expressions"),
            },
            Expression::PrefixExpression((t, expression_statement)) => {
                let right = eval(&Statement::ExpressStatement(
                    *(*expression_statement).clone(),
                ));

                if is_error(&right) {
                    return right;
                }

                eval_prefix_expression(&t, right)
            }
            Expression::InfixExpression((t, left, right)) => {
                let left = eval(&Statement::ExpressStatement(*(*left).clone()));
                if is_error(&left) {
                    return left;
                }

                let right = eval(&Statement::ExpressStatement(*(*right).clone()));
                if is_error(&right) {
                    return right;
                }

                eval_infix_statement(t, &left, &right)
            }
            Expression::IfExpression(condition, consequence, alt) => {
                eval_if_expression(&condition, &consequence, &alt)
            }
            _ => todo!(),
        },
        Statement::ReturnStatement(return_statement) => {
            let value = eval(&Statement::ExpressStatement(return_statement.value.clone()));

            if is_error(&value) {
                return value;
            }

            ObjectType::ReturnValueObj(Box::new(value))
        }
        _ => todo!(),
    }
}

fn is_error(obj: &ObjectType) -> bool {
    if *obj != NULL {
        std::mem::discriminant(&obj.r#type())
            == std::mem::discriminant(&ObjectType::ErrorObj(String::default()))
    } else {
        false
    }
}

fn new_error(msg: &str) -> ObjectType {
    ObjectType::ErrorObj(msg.to_string())
}

fn eval_block_statements(block: &BlockStatement) -> ObjectType {
    let mut result = ObjectType::default();

    for statement in block.statements.iter() {
        result = eval(&statement);

        let result_type = std::mem::discriminant(&result);
        if result != NULL
            && result_type
                == std::mem::discriminant(&ObjectType::ReturnValueObj(Box::new(
                    ObjectType::default(),
                )))
            || result_type == std::mem::discriminant(&ObjectType::ErrorObj(String::default()))
        {
            return result;
        }
    }

    result
}

fn eval_if_expression(
    conidition: &Expression,
    consequence: &BlockStatement,
    alt: &Option<Box<BlockStatement>>,
) -> ObjectType {
    let c = eval(&Statement::ExpressStatement(conidition.clone()));

    if is_error(&c) {
        return c;
    }

    if is_truthy(c) {
        return eval_block_statements(&consequence);
    } else if let Some(a) = alt {
        return eval_block_statements(&a);
    } else {
        NULL
    }
}

fn is_truthy(obj: ObjectType) -> bool {
    match obj {
        ObjectType::NullObj | ObjectType::BoolObj(false) => false,
        _ => true,
    }
}

fn eval_infix_statement(token: &Token, left: &ObjectType, right: &ObjectType) -> ObjectType {
    if std::mem::discriminant(&right.r#type()) != std::mem::discriminant(&left.r#type()) {
        return new_error(&format!(
            "type mismatch: {} {} {}",
            left.r#type(),
            token,
            right.r#type()
        ));
    }

    if let ObjectType::IntegerObj(int_left) = left {
        if let ObjectType::IntegerObj(int_right) = right {
            return eval_integer_infix_statement(token, int_left, int_right);
        }
    }

    match token {
        Token::EQ => native_bool_to_bool_obj(left == right),
        Token::NOT_EQ => native_bool_to_bool_obj(left != right),
        _ => new_error(&format!(
            "unknown operator: {} {} {}",
            left.r#type(),
            token,
            right.r#type()
        )),
    }
}

fn eval_integer_infix_statement(operator: &Token, left: &f64, right: &f64) -> ObjectType {
    match operator {
        Token::PLUS => ObjectType::IntegerObj(left + right),
        Token::MINUS => ObjectType::IntegerObj(left - right),
        Token::ASTERISK => ObjectType::IntegerObj(left * right),
        Token::SLASH => ObjectType::IntegerObj(left / right),
        Token::LT => native_bool_to_bool_obj(left < right),
        Token::GT => native_bool_to_bool_obj(left > right),
        Token::EQ => native_bool_to_bool_obj(left == right),
        Token::NOT_EQ => native_bool_to_bool_obj(left != right),
        _ => new_error(&format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_prefix_expression(operator: &Token, right: ObjectType) -> ObjectType {
    match operator {
        Token::BANG => eval_bang_operator(right),
        Token::MINUS => eval_minus_prefix(right),
        _ => new_error(&format!("unknown operator: {}{}", operator, right.r#type())),
    }
}

fn eval_minus_prefix(right: ObjectType) -> ObjectType {
    match right {
        ObjectType::IntegerObj(int) => ObjectType::IntegerObj(-int),
        _ => new_error(&format!("unknown operator: -{}", right.r#type())),
    }
}

fn eval_bang_operator(right: ObjectType) -> ObjectType {
    match right {
        ObjectType::BoolObj(value) => native_bool_to_bool_obj(!value),
        ObjectType::NullObj => TRUE,
        _ => FALSE,
    }
}

fn native_bool_to_bool_obj(input: bool) -> ObjectType {
    if input {
        TRUE
    } else {
        FALSE
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::ObjectType;
    use crate::parser::check_parse_errors;
    use crate::{object::Object, parser::Parser, test_setup};
    use core::panic;

    fn test_eval(input: &str) -> ObjectType {
        let program = test_setup!(input);

        eval_program(&program)
    }

    fn test_integer_object(object: &dyn Object, expected: f64) {
        match object.r#type() {
            ObjectType::IntegerObj(_) => {
                assert_eq!(object.inspect(), expected.to_string())
            }
            _ => panic!("Expected integer object, got {}", object.r#type()),
        }
    }

    fn test_bool_object(object: &dyn Object, expected: bool) {
        match object.r#type() {
            ObjectType::BoolObj(_) => {
                assert_eq!(object.inspect(), expected.to_string())
            }
            _ => panic!("Expected bool object"),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let input = vec![
            "5",
            "10",
            "-5",
            "-10",
            "5 + 5 + 5 + 5 - 10",
            "2 * 2 * 2 * 2 * 2",
            "-50 + 100 + -50",
            "5 * 2 + 10",
            "5 + 2 * 10",
            "20 + 2 * -10",
            "50 / 2 * 2 + 10",
            "2 * (5 + 10)",
            "3 * (3 * 3) + 10",
        ];
        let expected = vec![
            5.0, 10.0, -5.0, -10.0, 10.0, 32.0, 0.0, 20.0, 25.0, 0.0, 60.0, 30.0, 37.0,
        ];

        for (i, v) in input.iter().enumerate() {
            let evaluated = test_eval(v);
            test_integer_object(&evaluated, expected[i]);
        }
    }

    #[test]
    fn test_eval_bool() {
        let inputs = vec![
            "true",
            "false",
            "1 < 2",
            "1 > 2",
            "1 < 1",
            "1 > 1",
            "1 == 1",
            "1 != 1",
            "1 == 2",
            "1 != 2",
            "true == true",
            "false == false",
            "true == false",
            "true != false",
            "false != true",
            "(1 < 2) == true",
            "(1 < 2) == false",
        ];
        let expected = vec![
            true, false, true, false, false, false, true, false, false, true, true, true, false,
            true, true, true, false,
        ];

        for (i, v) in inputs.iter().enumerate() {
            let evalutated = test_eval(v);
            test_bool_object(&evalutated, expected[i]);
        }
    }

    #[test]
    fn test_bang_operator() {
        let inputs = vec!["!true", "!false", "!5", "!!true", "!!false", "!!5"];
        let expected = vec![false, true, false, true, false, true];

        for (i, v) in inputs.iter().enumerate() {
            let evaluated = test_eval(v);
            test_bool_object(&evaluated, expected[i]);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let inputs = vec![
            "if (true) { 10 }",
            "if (false) { 10 }",
            "if (1) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 > 2) { 10 }",
            "if (1 < 2) { 10 } else { 20 }",
            "if (1 > 2) { 10 } else { 20 }",
        ];
        let expected = vec![
            Some(10.0),
            None,
            Some(10.0),
            Some(10.0),
            None,
            Some(10.0),
            Some(20.0),
        ];

        for (i, v) in inputs.iter().enumerate() {
            let evaluted = test_eval(v);
            match evaluted {
                ObjectType::IntegerObj(int) => assert_eq!(int, expected[i].unwrap()),
                ObjectType::NullObj => assert!(expected[i].is_none()),
                _ => panic!("Only expected ints or nulls"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let inputs = vec![
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9;",
            "9; return 2 * 5; 9;",
            r#"if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }"#,
        ];
        let expected = [10.0; 5];

        for (i, v) in inputs.iter().enumerate() {
            let evaluated = test_eval(v);
            test_integer_object(&evaluated, expected[i]);
        }
    }

    #[test]
    fn test_error_handling() {
        let inputs = vec![
            "5 + true",
            "5 + true; 5",
            "-true",
            "true + false",
            "5; true + false; 5",
            "if (10 > 1) { true + false }",
            r#"if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                } 
                return 1;
            }"#,
        ];
        let expected = vec![
            "type mismatch: INTEGER + BOOLEAN",
            "type mismatch: INTEGER + BOOLEAN",
            "unknown operator: -BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
        ];

        for (i, v) in inputs.iter().enumerate() {
            let evaluated = test_eval(v);

            if let ObjectType::ErrorObj(error) = evaluated {
                assert_eq!(error, expected[i]);
            } else {
                panic!("Expect an error, got {evaluated:?}")
            }
        }
    }
}
