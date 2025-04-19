mod builtins;
use ast::{BlockStatement, Expression, Map, Program, Statement};
use builtins::BUILTINS;
use object::{Environment, Function, HashPair, Object, ObjectType};
use std::collections::HashMap;
use token::{Token, TokenLiteral};

const TRUE: ObjectType = ObjectType::BoolObj(true);
const FALSE: ObjectType = ObjectType::BoolObj(false);
const NULL: ObjectType = ObjectType::NullObj;

pub fn eval_program(program: &Program, env: &mut Environment) -> ObjectType {
    eval_statements(&program.statements, env)
}

pub fn eval_statements(statements: &Vec<Statement>, env: &mut Environment) -> ObjectType {
    let mut result = ObjectType::default();

    for statement in statements {
        result = eval(statement, env);

        if let ObjectType::ReturnValueObj(value) = result {
            return *value;
        }
        if let ObjectType::ErrorObj(_) = result {
            return result;
        }
    }

    result
}

pub fn eval(node: &Statement, env: &mut Environment) -> ObjectType {
    match node {
        Statement::ExpressStatement(expression) => eval_expression(expression, env),
        Statement::ReturnStatement(return_statement) => {
            let value = eval_expression(&return_statement.value, env);

            if is_error(&value) {
                return value;
            }

            ObjectType::ReturnValueObj(Box::new(value))
        }
        Statement::LetStatement(let_statement) => {
            let value = eval_expression(&let_statement.value, env);

            if is_error(&value) {
                return value;
            }

            env.set(&let_statement.name.token_literal(), value)
        }
        Statement::BlockStatement(block_statement) => eval_block_statements(block_statement, env),
    }
}

fn eval_expression(expression: &Expression, env: &mut Environment) -> ObjectType {
    match expression {
        Expression::IntExpression(int) => int.to_owned().into(),
        Expression::BoolExpression(bool) => bool.to_owned().into(),
        Expression::PrefixExpression((t, expression_statement)) => {
            let right = eval_expression(expression_statement, env);

            if is_error(&right) {
                return right;
            }

            eval_prefix_expression(t, right)
        }
        Expression::InfixExpression((t, left, right)) => {
            let left = eval_expression(left, env);
            if is_error(&left) {
                return left;
            }

            let right = eval_expression(right, env);
            if is_error(&right) {
                return right;
            }

            eval_infix_statement(t, &left, &right)
        }
        Expression::IfExpression(condition, consequence, alt) => {
            eval_if_expression(condition, consequence, alt, env)
        }
        Expression::IdentExpression(ident) => eval_ident(ident, env),
        Expression::FunctionLiteral(_, parameters, body, _) => ObjectType::FunctionObj(Function {
            parameters: parameters.to_vec(),
            body: body.clone(),
            // TODO: don't clone the env
            inner_env: env.clone(),
        }),
        Expression::CallExpression(func, args) => {
            let function = eval_expression(func, env);
            if is_error(&function) {
                return function;
            }

            let args = eval_expressions(args, env);
            if args.len() == 1 && is_error(&args[0]) {
                return args[0].clone();
            }

            apply_function(function, args, env)
        }
        Expression::StringExpression(token) => ObjectType::StringObj(token.token_literal()),
        Expression::ArrayExpression(array) => {
            let elements = eval_expressions(array, env);
            if elements.len() == 1 && is_error(&elements[0]) {
                return elements[0].clone();
            }
            ObjectType::ArrayObj(elements)
        }
        Expression::IndexExpression(left, index) => {
            let left = eval_expression(left, env);
            if is_error(&left) {
                return left;
            }

            let index = eval_expression(index, env);
            if is_error(&index) {
                return index;
            }

            eval_index_expression(left, index)
        }
        Expression::HashLiteral(map) => eval_hash_literal_node(map, env),
        _ => todo!(),
    }
}

fn eval_hash_literal_node(map: &Map, env: &mut Environment) -> ObjectType {
    let mut pairs = HashMap::new();
    for (k, v) in map.pairs.iter() {
        let key = eval_expression(k, env);
        if is_error(&key) {
            return key;
        }

        match key.hash() {
            Ok(hash_key) => {
                let value = eval_expression(v, env);
                if is_error(&value) {
                    return value;
                }

                pairs.insert(hash_key, HashPair { key, value });
            }
            Err(msg) => return new_error(&msg),
        };
    }

    ObjectType::HashObj(pairs)
}

fn eval_index_expression(left: ObjectType, index: ObjectType) -> ObjectType {
    if let ObjectType::ArrayObj(ref array) = left {
        if let ObjectType::IntegerObj(int) = index {
            return eval_array_index_expression(array, int);
        }
    }
    if let ObjectType::HashObj(map) = left {
        return eval_hash_index_expression(&map, index);
    }
    new_error(&format!("index operator not supported: {}", left.r#type()))
}

fn eval_hash_index_expression(map: &HashMap<u64, HashPair>, index: ObjectType) -> ObjectType {
    match index.hash() {
        Ok(hash_key) => {
            if let Some(v) = map.get(&hash_key) {
                v.value.clone()
            } else {
                NULL
            }
        }
        Err(msg) => new_error(&msg),
    }
}

fn eval_array_index_expression(array: &Vec<ObjectType>, index: f64) -> ObjectType {
    if index < 0.0 || index > array.len() as f64 - 1.0 {
        return NULL;
    }

    array[index as usize].clone()
}

fn apply_function(
    function: ObjectType,
    args: Vec<ObjectType>,
    env: &mut Environment,
) -> ObjectType {
    match function {
        ObjectType::FunctionObj(func) => {
            let mut extended_env = extend_func_env(func.clone(), args, env);
            let evaluated = eval(
                &Statement::BlockStatement(func.body.clone()),
                &mut extended_env,
            );
            unwrap_retrun_value(evaluated)
        }
        ObjectType::BuiltinFunction(builtin) => builtin(args),
        _ => new_error(&format!("not a function: {}", function.r#type())),
    }
}

fn extend_func_env(func: Function, args: Vec<ObjectType>, env: &mut Environment) -> Environment {
    let mut env = env.clone();
    env.inner_store = Some(Box::new(func.inner_env.clone()));

    for (i, param) in func.parameters.iter().enumerate() {
        env.set(&param.token_literal(), args[i].clone());
    }

    env
}

fn unwrap_retrun_value(obj: ObjectType) -> ObjectType {
    match obj {
        ObjectType::ReturnValueObj(return_value) => unwrap_retrun_value(*return_value),
        _ => obj,
    }
}

fn eval_expressions(expressions: &Vec<Expression>, env: &mut Environment) -> Vec<ObjectType> {
    let mut result = Vec::new();

    for expression in expressions {
        let evaluated = eval_expression(expression, env);
        if is_error(&evaluated) {
            return vec![evaluated];
        }

        result.push(evaluated);
    }

    result
}

fn eval_ident(token: &Token, env: &mut Environment) -> ObjectType {
    let name = token.token_literal();
    if let Some(value) = env.get(&name) {
        return value.clone();
    }

    if let Some(builtin) = BUILTINS.get(name.as_str()) {
        return ObjectType::BuiltinFunction(*builtin);
    }

    new_error(&format!("identifier not found: {name}"))
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

fn eval_block_statements(block: &BlockStatement, env: &mut Environment) -> ObjectType {
    let mut result = ObjectType::default();

    for statement in block.statements.iter() {
        result = eval(statement, env);

        let result_type = std::mem::discriminant(&result);
        if result != NULL
            && result_type == std::mem::discriminant(&ObjectType::ReturnValueObj(Box::default()))
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
    env: &mut Environment,
) -> ObjectType {
    let c = eval(&Statement::ExpressStatement(conidition.clone()), env);

    if is_error(&c) {
        return c;
    }

    if is_truthy(c) {
        eval_block_statements(consequence, env)
    } else if let Some(a) = alt {
        return eval_block_statements(a, env);
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

    if let ObjectType::StringObj(str_left) = left {
        if let ObjectType::StringObj(str_right) = right {
            return eval_string_infix_statement(token, str_left, str_right);
        }
    }

    match token {
        Token::Eq => native_bool_to_bool_obj(left == right),
        Token::Not_eq => native_bool_to_bool_obj(left != right),
        Token::Or => native_bool_to_bool_obj(left.to_native_bool() || right.to_native_bool()),
        _ => new_error(&format!(
            "unknown operator: {} {} {}",
            left.r#type(),
            token,
            right.r#type()
        )),
    }
}

fn eval_string_infix_statement(operator: &Token, left: &str, right: &str) -> ObjectType {
    match operator {
        Token::Plus => ObjectType::StringObj(format!("{left}{right}")),
        Token::Eq => ObjectType::BoolObj(left == right),
        Token::Not_eq => ObjectType::BoolObj(left != right),
        _ => new_error(&format!("unknown operator: STRING {} STRING", operator)),
    }
}

fn eval_integer_infix_statement(operator: &Token, left: &f64, right: &f64) -> ObjectType {
    match operator {
        Token::Plus => ObjectType::IntegerObj(left + right),
        Token::Minus => ObjectType::IntegerObj(left - right),
        Token::Asterisk => ObjectType::IntegerObj(left * right),
        Token::Slash => ObjectType::IntegerObj(left / right),
        Token::Lt => native_bool_to_bool_obj(left < right),
        Token::Gt => native_bool_to_bool_obj(left > right),
        Token::Eq => native_bool_to_bool_obj(left == right),
        Token::Not_eq => native_bool_to_bool_obj(left != right),
        _ => new_error(&format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_prefix_expression(operator: &Token, right: ObjectType) -> ObjectType {
    match operator {
        Token::Bang => eval_bang_operator(right),
        Token::Minus => eval_minus_prefix(right),
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
    if input { TRUE } else { FALSE }
}

#[cfg(test)]
mod test {
    use super::*;
    use core::panic;
    use lexer::Lexer;
    use object::Object;
    use object::ObjectType;
    use parser::{Parser, test_setup};

    fn test_eval(input: &str) -> ObjectType {
        let program = test_setup!(input);
        let mut env = Environment::new();

        eval_program(&program, &mut env)
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
            "foobar",
            "\"Hello\" - \"World\";",
        ];
        let expected = vec![
            "type mismatch: INTEGER + BOOLEAN",
            "type mismatch: INTEGER + BOOLEAN",
            "unknown operator: -BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "unknown operator: BOOLEAN + BOOLEAN",
            "identifier not found: foobar",
            "unknown operator: STRING - STRING",
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

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            "let a = 5; a;",
            "let a = 5 * 5 - 20; a;",
            "let a = 5; let b = a; b;",
            "let a = 5; let b = a; let c = a + b - 5; c;",
        ];

        for v in inputs {
            test_integer_object(&test_eval(v), 5.0);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        if let ObjectType::FunctionObj(func) = test_eval(&input) {
            assert_eq!(func.parameters.len(), 1);
            assert_eq!(func.parameters[0].token_literal(), "x");
            assert_eq!(func.body.to_string(), "(x + 2)");
        } else {
            panic!("object is not a function")
        }
    }

    #[test]
    fn test_function_application() {
        let inputs = vec![
            "let identity = fn(x) { x; }; identity(5);",
            "let identity = fn(x) { return x; }; identity(5);",
            "let double = fn(x) { x * 2}; double(5);",
            "let add = fn(x, y) { x + y; }; add(5, 5);",
            "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            "fn(x) { x; }(5)",
        ];
        let expected = vec![5.0, 5.0, 10.0, 10.0, 20.0, 5.0];

        for (i, v) in inputs.iter().enumerate() {
            test_integer_object(&test_eval(v), expected[i]);
        }
    }

    #[test]
    fn test_enclosures() {
        let input = r#"
        let newAdder = fn(x) {
            fn(y) { x + y };
        };

        let addTwo = newAdder(2);
        addTwo(2);"#;

        test_integer_object(&test_eval(&input), 4.0);
    }

    #[test]
    fn test_string_literal() {
        let input = "\"hello world!\"";

        match test_eval(input) {
            ObjectType::StringObj(s) => {
                assert_eq!(s, "hello world!");
            }
            _ => panic!("Expected string obj"),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\";";

        match test_eval(input) {
            ObjectType::StringObj(s) => {
                assert_eq!(s, "Hello World!");
            }
            _ => panic!("Expected string obj"),
        }
    }

    #[test]
    fn test_string_comparison() {
        let input = "\"Hello\" == \"Hello\"";
        test_bool_object(&test_eval(input), true);
    }

    #[test]
    fn test_string_comparison_false() {
        let input = "\"Hello\" == \"World\"";
        test_bool_object(&test_eval(input), false);
    }

    #[test]
    fn test_builtin_funcs() {
        let inputs = vec![
            "len(\"\")",
            "len(\"four\")",
            "len(\"hello world\")",
            "len(1)",
            "len(\"one\", \"two\")",
        ];

        let expected_int = [0.0, 4.0, 11.0];
        let expected_error = [
            "argument to `len` not supported, got INTEGER",
            "wrong number of arguments. got=2, want=1",
        ];

        for (i, input) in inputs.iter().enumerate() {
            match test_eval(input) {
                ObjectType::IntegerObj(e) => assert_eq!(e, expected_int[i]),
                ObjectType::ErrorObj(s) => assert_eq!(s, expected_error[i - 3]),
                _ => unreachable!("only ints and errors expected"),
            }
        }
    }

    #[test]
    fn test_array_builtin_funcs() {
        let inputs = vec![
            "len([1,2,0])",
            "len([])",
            "first([3,2,1])",
            "last([1,3,2,5])",
        ];

        let expected_int = [3.0, 0.0, 3.0, 5.0];

        for (i, input) in inputs.iter().enumerate() {
            match test_eval(input) {
                ObjectType::IntegerObj(e) => assert_eq!(e, expected_int[i]),
                _ => unreachable!("only ints expected"),
            }
        }
    }

    #[test]
    fn test_array_rest_builtin() {
        let input = "rest([1,2,3,4,5])";

        match test_eval(input) {
            ObjectType::ArrayObj(array) => assert_eq!(
                array
                    .iter()
                    .map(|obj| match obj {
                        ObjectType::IntegerObj(v) => v.to_owned(),
                        _ => unreachable!("only ints in array expected"),
                    })
                    .collect::<Vec<_>>(),
                vec![2.0, 3.0, 4.0, 5.0]
            ),
            _ => unreachable!("only array objects expected"),
        }
    }

    #[test]
    fn test_array_builtin_funcs_that_return_null() {
        let inputs = vec!["first([])", "last([])", "rest([])"];

        for input in inputs {
            match test_eval(input) {
                ObjectType::NullObj => {}
                _ => unreachable!("only null objects expected"),
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        match test_eval(input) {
            ObjectType::ArrayObj(array) => {
                assert_eq!(array.len(), 3);
                test_integer_object(&array[0], 1.0);
                test_integer_object(&array[1], 4.0);
                test_integer_object(&array[2], 6.0);
            }
            _ => unreachable!("expected an array obj"),
        }
    }

    #[test]
    fn test_index_expressions() {
        let inputs = vec![
            ("[1,2,3][0]", 1.0),
            ("[1,2,3][1]", 2.0),
            ("[1,2,3][2]", 3.0),
        ];

        for (i, v) in inputs.iter().enumerate() {
            match test_eval(v.0) {
                ObjectType::IntegerObj(int) => assert_eq!(int, inputs[i].1),
                _ => unreachable!("expected an integer object"),
            }
        }
    }

    #[test]
    fn test_out_of_bounds_index() {
        let input = "[1,2][2]";
        assert_eq!(test_eval(input), ObjectType::NullObj)
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
            let two = "two";
            {
                "one": 10 - 9,
                "two": 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6,
            };
        "#;

        match test_eval(input) {
            ObjectType::HashObj(map) => {
                assert_eq!(map.len(), 6);
                test_integer_object(
                    &map.get(&ObjectType::StringObj("one".to_string()).hash().unwrap())
                        .unwrap()
                        .value,
                    1.0,
                );
                test_integer_object(
                    &map.get(&ObjectType::StringObj("two".to_string()).hash().unwrap())
                        .unwrap()
                        .value,
                    2.0,
                );
                test_integer_object(
                    &map.get(&ObjectType::StringObj("three".to_string()).hash().unwrap())
                        .unwrap()
                        .value,
                    3.0,
                );
                test_integer_object(
                    &map.get(&ObjectType::IntegerObj(4.0).hash().unwrap())
                        .unwrap()
                        .value,
                    4.0,
                );
                test_integer_object(
                    &map.get(&ObjectType::BoolObj(true).hash().unwrap())
                        .unwrap()
                        .value,
                    5.0,
                );
                test_integer_object(
                    &map.get(&ObjectType::BoolObj(false).hash().unwrap())
                        .unwrap()
                        .value,
                    6.0,
                );
            }
            _ => panic!("expected hash object"),
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let input = "{\"foo\": 5}[\"foo\"];";
        test_integer_object(&test_eval(input), 5.0);
    }

    #[test]
    fn test_hash_index_expressions_with_bool_key() {
        let input = "{true: 5}[true];";
        test_integer_object(&test_eval(input), 5.0);
    }

    #[test]
    fn test_hash_index_expressions_with_string_key() {
        let input = "let key = \"foo\"; {\"foo\": 5}[key];";
        test_integer_object(&test_eval(input), 5.0);
    }

    #[test]
    fn test_hash_index_expressions_with_null() {
        let input = "{}[\"key\"];";
        assert_eq!(test_eval(input), ObjectType::NullObj);
    }

    #[test]
    fn test_hash_index_expressions_with_int() {
        let input = "{5: 5}[5];";
        test_integer_object(&test_eval(input), 5.0);
    }

    #[test]
    fn test_infix_or() {
        let input = "true || false;";
        test_bool_object(&test_eval(input), true);

        let input = "false || false;";
        test_bool_object(&test_eval(input), false);
    }
}
