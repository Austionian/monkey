use crate::{
    ast::{Program, Statement},
    object::{Integer, Object},
    token::Token,
};

pub fn eval_statements(program: &Program) -> Option<impl Object> {
    let mut result = None;
    for statement in program.statements.iter() {
        result = eval(&statement);
    }

    result
}

pub fn eval(node: &Statement) -> Option<impl Object> {
    match node {
        Statement::ExpressStatement(expression) => match &expression.value {
            crate::ast::Expression::IntExpression(int) => match int {
                Token::INT(v) => Some(Integer { value: *v as f64 }),
                _ => unreachable!("Only ints belong in int expressions"),
            },
            _ => todo!(),
        },
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::ObjectType;
    use crate::parser::check_parse_errors;
    use crate::{object::Object, parser::Parser, test_setup};

    fn test_eval(input: &str) -> Option<impl Object> {
        let program = test_setup!(input);

        eval_statements(&program)
    }

    fn test_integer_object(object: &dyn Object, expected: usize) {
        match object.r#type() {
            ObjectType::IntegerObj => {
                assert_eq!(object.inspect(), expected.to_string())
            }
            _ => panic!("Expected integer object"),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let input = vec!["5", "10"];
        let expected = vec![5, 10];

        for (i, v) in input.iter().enumerate() {
            let evaluated = test_eval(v).unwrap();
            test_integer_object(&evaluated, expected[i])
        }
    }
}
