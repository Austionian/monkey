use crate::parse_errors;
use evaluator::eval_program;
use object::{Environment, Object};
use parser::Parser;
use std::{
    io::{self, Write},
    process::{self},
};

const PROMPT: &str = ">> ";

pub fn repl_start(env: &mut Environment) {
    print!("{PROMPT}");
    let _ = io::stdout().flush();

    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();

    if buffer.trim() == "/bye" || buffer.trim() == "exit" {
        process::exit(0)
    }

    let lexer = lexer::Lexer::new(&buffer);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    parse_errors!(parser);

    if let Ok(program) = program {
        let evaluated = eval_program(&program, env);
        println!("{}", evaluated.inspect());
    }
}
