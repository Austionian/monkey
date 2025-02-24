use crate::{
    ast,
    code::{self, Opcode, OP_CONSTANT},
    object,
};

struct Compiler {
    instructions: code::Instructions,
    constants: Vec<object::ObjectType>,
}

type ByteCode = Compiler;

struct CompilerError {}

impl Compiler {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }

    fn compile(mut self, node: ast::Program) -> Result<Self, CompilerError> {
        for statement in node.statements {
            self.compile_statement(statement)?;
        }

        Ok(self)
    }

    fn compile_statement(&mut self, statement: ast::Statement) -> Result<(), CompilerError> {
        match statement {
            ast::Statement::ExpressStatement(exp) => Ok(self.compile_expression(&exp)?),
            ast::Statement::LetStatement(exp) => Ok(Compiler::compile_let_statement(exp)?),
            ast::Statement::BlockStatement(exp) => Ok(Compiler::compile_block(exp)?),
            ast::Statement::ReturnStatement(exp) => Ok(Compiler::compile_return(exp)?),
        }
    }

    fn compile_return(expression: ast::ReturnStatement) -> Result<(), CompilerError> {
        todo!()
    }

    fn compile_block(expression: ast::BlockStatement) -> Result<(), CompilerError> {
        todo!()
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), CompilerError> {
        match expression {
            ast::Expression::IntExpression(t) => {
                let integer = t.to_owned().into();
                let i = self.add_constant(integer);
                let _ = self.emit(&OP_CONSTANT, vec![i]);
            }
            ast::Expression::InfixExpression((_, left, right)) => {
                self.compile_expression(left.as_ref())?;
                self.compile_expression(right.as_ref())?;
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn add_constant(&mut self, obj: object::ObjectType) -> usize {
        self.constants.push(obj);
        return self.constants.len() - 1;
    }

    fn emit(&mut self, op: &Opcode, operands: Vec<usize>) -> usize {
        // TODO: remove this map, or how this mapping be done when it gets here?
        let mut ins = code::make(op, operands.iter().map(|x| *x as u16));
        self.add_instruction(&mut ins)
    }

    fn add_instruction(&mut self, ins: &mut Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.append(ins);

        pos_new_instruction
    }

    fn compile_let_statement(statement: ast::LetStatement) -> Result<(), CompilerError> {
        todo!()
    }

    fn bytecode(self) -> ByteCode {
        ByteCode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser, test_setup};
    use core::panic;
    use object::ObjectType;

    enum CompilerInterface {
        Int(f64),
    }

    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<CompilerInterface>,
        expected_instructions: Vec<code::Instructions>,
    }

    /// helper function
    fn concat_instructions(expected: Vec<code::Instructions>) -> code::Instructions {
        expected.into_iter().flatten().collect()
    }
    fn test_instructions(expected: Vec<code::Instructions>, actual: code::Instructions) {
        let concatted = concat_instructions(expected);

        assert_eq!(concatted.len(), actual.len());

        for (i, instruction) in actual.iter().enumerate() {
            assert_eq!(instruction, &concatted[i])
        }
    }

    fn test_integer_object(expected: f64, actual: &object::ObjectType) {
        match actual {
            ObjectType::IntegerObj(x) => assert_eq!(expected, *x),
            _ => panic!("expected only integer objects"),
        }
    }

    fn test_constants(expected: Vec<CompilerInterface>, actual: Vec<ObjectType>) {
        assert_eq!(expected.len(), actual.len());

        for (i, constant) in expected.iter().enumerate() {
            match constant {
                CompilerInterface::Int(x) => test_integer_object(*x, &actual[i]),
            }
        }
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let program = test_setup!(&test.input);
            let compiler = Compiler::new();

            if let Ok(bytecode) = compiler.compile(program) {
                test_instructions(test.expected_instructions, bytecode.instructions);
                test_constants(test.expected_constants, bytecode.constants);
            } else {
                panic!("expected the program to compile to bytecode");
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![CompilerTestCase {
            input: "1 + 2".to_string(),
            expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
            expected_instructions: vec![
                code::make(&code::OP_CONSTANT, vec![0u16]),
                code::make(&code::OP_CONSTANT, vec![1u16]),
            ],
        }];

        run_compiler_tests(tests);
    }
}
