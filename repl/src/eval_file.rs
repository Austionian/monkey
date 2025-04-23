use crate::parse_errors;
use evaluator::eval_program;
use object::{Environment, Object};
use parser::Parser;

pub fn eval(env: &mut Environment, buffer: &str) {
    let lexer = lexer::Lexer::new(buffer);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    parse_errors!(parser);

    if let Ok(program) = program {
        let evaluated = eval_program(&program, env);
        println!("{}", evaluated.inspect());
    }
}
