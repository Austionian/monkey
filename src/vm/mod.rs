use crate::{
    code::{self, Opcode},
    compiler::ByteCode,
    object::{Object, ObjectType},
};
use std::{any::Any, mem};

const STACK_SIZE: usize = 2048;

pub struct VM {
    constants: Vec<ObjectType>,
    instructions: code::Instructions,
    stack: [ObjectType; STACK_SIZE],
    // stack pointer
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
            let op: Opcode = self.instructions[ip];

            match op {
                code::OP_CONSTANT => {
                    let const_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;

                    // TODO: remove this clone and cast
                    self.push(self.constants[const_index as usize].clone())?;
                }
                code::OP_ADD | code::OP_SUB | code::OP_MUL | code::OP_DIV => {
                    self.execute_binary_operation(&op)?
                }
                code::OP_POP => {
                    self.pop();
                }
                _ => todo!(),
            }

            ip += 1;
        }

        Ok(())
    }

    fn execute_binary_operation(&mut self, op: &Opcode) -> Result<(), String> {
        if let ObjectType::IntegerObj(right) = self.pop() {
            if let ObjectType::IntegerObj(left) = self.pop() {
                self.execute_binary_int_operation(op, left, right)?;
            } else {
                todo!();
            };
        } else {
            todo!();
        };

        Ok(())
    }

    fn execute_binary_int_operation(
        &mut self,
        op: &Opcode,
        left: f64,
        right: f64,
    ) -> Result<(), String> {
        match op {
            &code::OP_ADD => self.push(ObjectType::IntegerObj(left + right)),
            &code::OP_SUB => self.push(ObjectType::IntegerObj(left - right)),
            &code::OP_MUL => self.push(ObjectType::IntegerObj(left * right)),
            &code::OP_DIV => self.push(ObjectType::IntegerObj(left / right)),

            _ => return Err(format!("Unsupported integer operator: {}", op)),
        }
    }

    fn push(&mut self, o: ObjectType) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            return Err("stack overflow".to_string());
        }

        self.stack[self.sp] = o;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> ObjectType {
        let o = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        o
    }

    pub fn stack_top(&self) -> Option<&ObjectType> {
        if self.sp == 0 {
            None
        } else {
            Some(&self.stack[self.sp - 1])
        }
    }

    pub fn last_popped_stack_elem(&self) -> ObjectType {
        self.stack[self.sp].clone()
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

            let stack_elem = vm.last_popped_stack_elem();
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
                expected: Box::new(3.0f64),
            },
            VmTestCase {
                input: "1 - 2".to_string(),
                expected: Box::new(-1.0f64),
            },
            VmTestCase {
                input: "1 * 2".to_string(),
                expected: Box::new(2.0f64),
            },
            VmTestCase {
                input: "2 / 1".to_string(),
                expected: Box::new(2.0f64),
            },
            VmTestCase {
                input: "5 * (2 + 10)".to_string(),
                expected: Box::new(60.0f64),
            },
        ];

        run_vm_tests(tests);
    }
}
