use monkey_interpreter::{
    compiler::symbol_table::SymbolTable, object::ObjectType, repl, vm::GLOBAL_SIZE,
};

fn main() {
    println!("Hello, this is the Monkey programming language!");
    println!("Feel free to type in commands:");
    let mut constants = Vec::new();
    let mut symbol_table = SymbolTable::new();
    let mut globals = [const { ObjectType::NullObj }; GLOBAL_SIZE];

    loop {
        repl::start(&mut constants, &mut symbol_table, &mut globals);
    }
}
