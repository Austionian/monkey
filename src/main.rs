use clap::Parser;
use monkey_interpreter::{
    compiler::symbol_table::SymbolTable,
    object::Environment,
    object::{Object, ObjectType},
    repl,
    vm::GLOBAL_SIZE,
};
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Optional mode to run in, default to repl
    mode: Option<String>,

    /// Optional path to file to be run
    #[arg(long)]
    path: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    match args.mode.unwrap_or("repl".into()).as_str() {
        "repl" => start(),
        "eval" => {
            if args.path.is_some() {
                match std::fs::read_to_string(args.path.unwrap()) {
                    Ok(file) => {
                        let mut env = Environment::new();
                        crate::repl::eval(&mut env, &file);
                    }
                    Err(_) => eprintln!("no such file"),
                }
            } else {
                loop {
                    let mut env = Environment::new();
                    crate::repl::repl_start(&mut env);
                }
            }
        }
        "compile" => {
            if args.path.is_some() {
                match std::fs::read_to_string(args.path.unwrap()) {
                    Ok(file) => {
                        let mut constants = Vec::new();
                        let symbol_table = SymbolTable::new();
                        let mut globals = [const { ObjectType::NullObj }; GLOBAL_SIZE];

                        repl::compile(&mut constants, symbol_table, &mut globals, &file);
                    }
                    Err(_) => eprintln!("no such file"),
                }
            } else {
                start();
            }
        }
        // the default is "repl" if no option is given
        _ => unreachable!(),
    }
}

/// Starts the repl using the bytecode compiler and vm.
fn start() {
    println!("Hello, this is the Monkey programming language!");
    println!("Feel free to type in commands:");
    let mut constants = Vec::new();
    let mut symbol_table = SymbolTable::new();
    let mut globals = [const { ObjectType::NullObj }; GLOBAL_SIZE];

    loop {
        symbol_table = repl::start(&mut constants, symbol_table, &mut globals);
    }
}
