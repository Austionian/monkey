mod compile;
mod compile_file;
mod eval;
mod eval_file;

pub use compile::repl_compiler;
pub use compile_file::compile;
pub use eval::repl_start;
pub use eval_file::eval;

#[macro_export]
macro_rules! read {
    () => {{
        use std::{
            io::{self, Write},
            process::{self},
        };

        print!("{}", $crate::PROMPT);
        let _ = io::stdout().flush();

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();

        if buffer.trim() == "/bye" || buffer.trim() == "exit" {
            process::exit(0)
        }

        buffer
    }};
}

#[macro_export]
macro_rules! parse_errors {
    ($p:expr) => {{
        if !$p.errors.is_empty() {
            eprintln!("{}", $crate::MONKEY_FACE);
            eprintln!("Whoops! We ran into some monkey business here!");
            eprintln!("parser errors:");
            for error in $p.errors.iter() {
                eprintln!("\t{error}");
            }
            return;
        }
    }};

    ($p:expr, $symbol_table:expr) => {{
        if !$p.errors.is_empty() {
            eprintln!("{}", $crate::MONKEY_FACE);
            eprintln!("Whoops! We ran into some monkey business here!");
            eprintln!("parser errors:");
            for error in $p.errors.iter() {
                eprintln!("\t{error}");
            }
            return $symbol_table;
        }
    }};
}

#[macro_export]
macro_rules! start {
    ($buffer:expr) => {{
        let lexer = lexer::Lexer::new($buffer);
        let mut parser = parser::Parser::new(lexer);

        let program = parser.parse_program();

        $crate::parse_errors!(parser);

        program
    }};

    ($buffer:expr, $symbol_table:expr) => {{
        let lexer = lexer::Lexer::new($buffer);
        let mut parser = parser::Parser::new(lexer);

        let program = parser.parse_program();

        $crate::parse_errors!(parser, $symbol_table);

        program
    }};
}

#[macro_export]
macro_rules! evaluate {
    ($buffer:expr, $env:expr) => {{
        use object::Object;

        let program = $crate::start!($buffer);

        if let Ok(program) = program {
            let evaluated = evaluator::eval_program(&program, $env);
            println!("{}", evaluated.inspect());
        }
    }};
}

pub const PROMPT: &str = ">> ";

pub const MONKEY_FACE: &str = r#"            __,__
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
