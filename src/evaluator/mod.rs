use crate::{
    ast::{Expression, Program, Statement},
    object::{Boolean, Integer, Null, ObjectType},
    token::Token,
};

pub fn eval_statements(program: &Program) -> Option<ObjectType> {
    let mut result = None;
    for statement in program.statements.iter() {
        result = eval(&statement);
    }

    result
}

const TRUE: ObjectType = ObjectType::BoolObj(Boolean { value: true });
const FALSE: ObjectType = ObjectType::BoolObj(Boolean { value: false });
const NULL: ObjectType = ObjectType::NullObj(Null {});

pub fn eval(node: &Statement) -> Option<ObjectType> {
    match node {
        Statement::ExpressStatement(expression) => match &expression.value {
            Expression::IntExpression(int) => match int {
                Token::INT(v) => Some(ObjectType::IntegerObj(Integer { value: *v as f64 })),
                _ => unreachable!("Only ints belong in int expressions"),
            },
            Expression::BoolExpression(bool) => match bool {
                Token::TRUE => Some(TRUE),
                Token::FALSE => Some(FALSE),
                _ => unreachable!("Only bools belong in bool expressions"),
            },
            Expression::PrefixExpression((t, expression_statement)) => {
                let right = eval(&Statement::ExpressStatement(
                    *(*expression_statement).clone(),
                ))?;
                Some(eval_prefix_expression(&t, right))
            }
            _ => todo!(),
        },
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: &Token, right: ObjectType) -> ObjectType {
    match operator {
        Token::BANG => eval_bang_operator(right),
        Token::MINUS => eval_minus_prefix(right),
        _ => NULL,
    }
}

fn eval_minus_prefix(right: ObjectType) -> ObjectType {
    match right {
        ObjectType::IntegerObj(int) => ObjectType::IntegerObj(Integer { value: -int.value }),
        _ => NULL,
    }
}

fn eval_bang_operator(right: ObjectType) -> ObjectType {
    match right {
        ObjectType::BoolObj(Boolean { value }) => {
            if value {
                FALSE
            } else {
                TRUE
            }
        }
        ObjectType::NullObj(_) => TRUE,
        _ => FALSE,
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

    fn test_eval(input: &str) -> Option<ObjectType> {
        let program = test_setup!(input);

        eval_statements(&program)
    }

    fn test_integer_object(object: &dyn Object, expected: f64) {
        match object.r#type() {
            ObjectType::IntegerObj(_) => {
                assert_eq!(object.inspect(), expected.to_string())
            }
            _ => panic!("Expected integer object"),
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
        let input = vec!["5", "10", "-5", "-10"];
        let expected = vec![5.0, 10.0, -5.0, -10.0];

        for (i, v) in input.iter().enumerate() {
            let evaluated = test_eval(v).unwrap();
            test_integer_object(&evaluated, expected[i]);
        }
    }

    #[test]
    fn test_eval_bool() {
        let inputs = vec!["true", "false"];
        let expected = vec![true, false];

        for (i, v) in inputs.iter().enumerate() {
            let evalutated = test_eval(v).unwrap();
            test_bool_object(&evalutated, expected[i]);
        }
    }

    #[test]
    fn test_bang_operator() {
        let inputs = vec!["!true", "!false", "!5", "!!true", "!!false", "!!5"];
        let expected = vec![false, true, false, true, false, true];

        for (i, v) in inputs.iter().enumerate() {
            let evaluated = test_eval(v).unwrap();
            test_bool_object(&evaluated, expected[i]);
        }
    }
}
