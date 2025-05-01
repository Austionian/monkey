use clap::{Parser, ValueEnum};
use compiler::symbol_table::SymbolTable;
use object::{Environment, ObjectType};
use std::path::PathBuf;
use vm::GLOBAL_SIZE;

#[derive(ValueEnum, Clone, Default)]
#[clap(rename_all = "kebab_case")]
enum Mode {
    #[default]
    Compile,
    Eval,
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Optional path to file to be run, running without will start the repl
    path: Option<PathBuf>,

    #[arg(long)]
    /// Optional mode to run in, defaults to compile
    mode: Option<Mode>,
}

fn main() {
    let args = Args::parse();
    match args.path {
        Some(path) => match args.mode.unwrap_or_default() {
            Mode::Compile => match std::fs::read_to_string(path) {
                Ok(file) => {
                    let mut constants = Vec::new();
                    let symbol_table = SymbolTable::new();
                    let mut globals = [const { ObjectType::NullObj }; GLOBAL_SIZE];

                    repl::compile(&mut constants, symbol_table, &mut globals, &file);
                }
                Err(_) => eprintln!("no such file"),
            },
            Mode::Eval => match std::fs::read_to_string(path) {
                Ok(file) => {
                    let mut env = Environment::new();
                    repl::eval(&mut env, &file);
                }
                Err(_) => eprintln!("no such file"),
            },
        },
        None => match args.mode.unwrap_or_default() {
            Mode::Compile => start(),
            Mode::Eval => {
                let mut env = Environment::new();
                loop {
                    repl::repl_start(&mut env);
                }
            }
        },
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
        symbol_table = repl::repl_compiler(&mut constants, symbol_table, &mut globals);
    }
}
