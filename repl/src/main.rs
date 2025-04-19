use clap::{Parser, ValueEnum};
use compiler::symbol_table::SymbolTable;
use object::{Environment, ObjectType};
use std::path::PathBuf;
use vm::GLOBAL_SIZE;

#[derive(ValueEnum, Clone, Default)]
#[clap(rename_all = "kebab_case")]
enum Mode {
    #[default]
    Repl,
    Compile,
    Eval,
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Optional mode to run in, default to repl
    mode: Option<Mode>,

    /// Optional path to file to be run
    #[arg(long)]
    path: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    match args.mode.unwrap_or(Mode::Repl) {
        Mode::Repl => start(),
        Mode::Eval => {
            if args.path.is_some() {
                match std::fs::read_to_string(args.path.unwrap()) {
                    Ok(file) => {
                        let mut env = Environment::new();
                        repl::eval(&mut env, &file);
                    }
                    Err(_) => eprintln!("no such file"),
                }
            } else {
                loop {
                    let mut env = Environment::new();
                    repl::repl_start(&mut env);
                }
            }
        }
        Mode::Compile => {
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
