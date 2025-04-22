use compiler::{Compiler, symbol_table::SymbolTable};
use lexer::Lexer;
use object::{Object, ObjectType};
use parser::Parser;
use vm::{GLOBAL_SIZE, VM};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
}

#[wasm_bindgen]
pub fn print(name: &str) {
    alert(&format!("result:, {}", name));
}

#[wasm_bindgen]
pub fn execute(buffer: &str) -> String {
    let mut constants = Vec::new();
    let symbol_table = SymbolTable::new();
    let mut globals = [const { ObjectType::NullObj }; GLOBAL_SIZE];
    let lexer = Lexer::new(buffer);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        let mut error = String::new();
        error.push_str("Whoops! We ran into some monkey business here!\n");
        error.push_str("parser errors:\n");
        for e in parser.errors.into_iter() {
            error.push_str(format!("\t{e}\n").as_str());
        }

        return error;
    }

    if let Ok(program) = program {
        let mut comp = Compiler::new(&mut constants, symbol_table);
        if comp.compile(program).is_err() {
            eprintln!("woops! compilation failed");
        }

        let mut machine = VM::new(comp, &mut globals);
        if let Err(e) = machine.run() {
            eprintln!("whoops! executing the bytecode failed:, {e}");
        }

        let stack_top = machine.last_popped_stack_elem();

        return stack_top.inspect();
    }

    String::from("Not sure how you ended up here, friend, but welcome.")
}
