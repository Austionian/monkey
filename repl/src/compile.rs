use crate::parse_errors;
use compiler::{Compiler, symbol_table::SymbolTable};
use object::{Object, ObjectType};
use parser::Parser;
use vm::{GLOBAL_SIZE, VM};

pub fn compile(
    constants: &mut Vec<ObjectType>,
    symbol_table: SymbolTable,
    globals: &mut [ObjectType; GLOBAL_SIZE],
    buffer: &str,
) -> SymbolTable {
    let lexer = lexer::Lexer::new(buffer);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    parse_errors!(parser, symbol_table);

    if let Ok(program) = program {
        let mut comp = Compiler::new(constants, symbol_table);
        if comp.compile(program).is_err() {
            eprintln!("woops! compilation failed");
        }

        let symbols = comp.symbol_table.clone();
        let mut machine = VM::new(comp, globals);
        if let Err(e) = machine.run() {
            eprintln!("whoops! executing the bytecode failed - {e}");
        }

        let stack_top = machine.last_popped_stack_elem();
        println!("{}", stack_top.inspect());

        return symbols;
    }

    symbol_table
}
