use crate::{
    compiler::{symbol_table::SymbolTable, Compiler},
    lexer,
    object::{Object, ObjectType},
    parser::Parser,
    vm::{GLOBAL_SIZE, VM},
};
use std::{
    io::{self, Write},
    process::{self},
};

const PROMPT: &'static str = ">> ";

pub fn start<'a, 'b>(
    constants: &'a mut Vec<ObjectType>,
    // 'b outlives 'a but this probably isn't sustainable because 'b is pretty
    // much static here. New scope will be thrown away on subsequent iterations
    // of the loop, but they'll still probably be expected to have 'b lifetime
    symbol_table: &'a mut SymbolTable<'b>,
    globals: &mut [ObjectType; GLOBAL_SIZE],
) where
    'b: 'a,
{
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
        let mut comp = Compiler::new(constants, symbol_table);
        if let Err(_) = comp.compile(program) {
            eprintln!("woops! compilation failed");
        }

        let mut machine: VM = VM::new(comp, globals);
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
