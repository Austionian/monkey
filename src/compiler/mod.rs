use crate::{ast, code, object};

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

    fn compile(self, node: ast::Program) -> Result<Self, CompilerError> {
        Err(CompilerError {})
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
                code::make(&code::OP_CONSTANT, vec![0]),
                code::make(&code::OP_CONSTANT, vec![1]),
            ],
        }];

        run_compiler_tests(tests);
    }
}
