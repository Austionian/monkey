use crate::{
    code::{self, Opcode},
    compiler::ByteCode,
    object::ObjectType,
};

const STACK_SIZE: usize = 2048;

struct VM {
    constants: Vec<ObjectType>,
    instructions: code::Instructions,
    stack: [ObjectType; STACK_SIZE],
    sp: usize,
}

impl VM {
    pub fn new(bytecode: ByteCode) -> Self {
        VM {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: [const { ObjectType::NullObj }; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        // ip = 'instruction pointer'
        let mut ip = 0;
        while ip < self.instructions.len() {
            println!(
                "ip is {ip}, len is {}, {:?}",
                self.instructions.len(),
                self.instructions
            );

            let op: Opcode = self.instructions[ip];

            match op {
                code::OP_CONSTANT => {
                    let const_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;

                    // TODO: remove this clone and cast
                    self.push(self.constants[const_index as usize].clone())?;
                }
                _ => todo!(),
            }

            ip += 1;
        }

        Ok(())
    }

    fn push(&mut self, o: ObjectType) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            return Err("stack overflow".to_string());
        }

        self.stack[self.sp] = o;
        self.sp += 1;

        Ok(())
    }

    pub fn stack_top(&self) -> Option<&ObjectType> {
        if self.sp == 0 {
            None
        } else {
            Some(&self.stack[self.sp - 1])
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        code,
        compiler::Compiler,
        lexer::Lexer,
        object::{self, ObjectType},
        parser::Parser,
        test_setup,
    };
    use core::panic;
    use std::any::Any;

    enum CompilerInterface {
        Int(f64),
    }

    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<CompilerInterface>,
        expected_instructions: Vec<code::Instructions>,
    }

    struct VmTestCase {
        input: String,
        expected: Box<dyn Any>,
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for test in tests {
            let program = test_setup!(&test.input);
            let mut comp = Compiler::new();

            comp.compile(program).unwrap();

            let mut vm = VM::new(comp.bytecode());
            vm.run().unwrap();

            let stack_elem = vm.stack_top().unwrap();
            test_expected_object(test.expected, &stack_elem);
        }
    }

    fn test_expected_object(expected: Box<dyn Any>, actual: &object::ObjectType) {
        if expected.is::<f64>() {
            test_integer_object(*expected.downcast::<f64>().unwrap(), actual);
            return;
        }

        panic!("expected f64");
    }

    fn test_integer_object(expected: f64, actual: &object::ObjectType) {
        match actual {
            ObjectType::IntegerObj(x) => assert_eq!(expected, *x),
            _ => panic!("expected only integer objects"),
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VmTestCase> = vec![
            VmTestCase {
                input: "1".to_string(),
                expected: Box::new(1.0f64),
            },
            VmTestCase {
                input: "2".to_string(),
                expected: Box::new(2.0f64),
            },
            VmTestCase {
                input: "1 + 2".to_string(),
                expected: Box::new(2.0f64),
            },
        ];

        run_vm_tests(tests);
    }
}
