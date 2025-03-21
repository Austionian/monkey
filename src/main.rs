use monkey_interpreter::{compiler::symbol_table::SymbolTable, repl};

fn main() {
    println!("Hello, this is the Monkey programming language!");
    println!("Feel free to type in commands:");
    let mut constants = Vec::new();
    let mut symbol_table = SymbolTable::new();

    loop {
        repl::start(&mut constants, &mut symbol_table);
    }
}
