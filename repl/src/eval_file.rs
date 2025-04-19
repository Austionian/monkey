use evaluator::eval_program;
use object::{Environment, Object};
use parser::Parser;

pub fn eval(env: &mut Environment, buffer: &str) {
    let lexer = lexer::Lexer::new(buffer);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        eprintln!("Whoops! We ran into some monkey business here!");
        eprintln!("parser errors:");
        for error in parser.errors {
            eprintln!("\t{error}");
        }
        return;
    }

    if let Ok(program) = program {
        let evaluated = eval_program(&program, env);
        println!("{}", evaluated.inspect());
    }
}
