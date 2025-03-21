use crate::{
    compiler::Compiler,
    lexer,
    object::{Environment, Object},
    parser::Parser,
    vm::VM,
};
use std::{
    io::{self, Write},
    process::{self},
};

const PROMPT: &'static str = ">> ";

pub fn start(env: &mut Environment) {
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

    if !parser.errors.is_empty() {
        eprintln!("{MONKEY_FACE}");
        eprintln!("Whoops! We ran into some monkey business here!");
        eprintln!("parser errors:");
        for error in parser.errors {
            eprintln!("\t{error}");
        }
        return;
    }

    // evaluated program
    //if let Ok(program) = program {
    //    let evaluated = eval_program(&program, env);
    //    println!("{}", evaluated.inspect());
    //}
    //let mut tok = Token::default();
    //while tok != Token::EOF {
    //    tok = lexer.next_token();
    //    println!("out: {:?}", tok);
    //}

    if let Ok(program) = program {
        let mut comp = Compiler::new();
        if let Err(_) = comp.compile(program) {
            eprintln!("woops! compilation failed");
        }

        let mut machine = VM::new(comp.bytecode());
        if let Err(e) = machine.run() {
            eprintln!("whoops! executing the bytecode failed:, {e}");
        }

        let stack_top = machine.last_popped_stack_elem();
        println!("{}", stack_top.inspect());
    }
}

const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;
