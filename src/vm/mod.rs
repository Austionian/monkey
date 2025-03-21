use crate::{
    code::{self, Opcode},
    compiler::Compiler,
    object::ObjectType,
};
use anyhow;

const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 100; // TODO: look into why this can't be 65536
const TRUE: ObjectType = ObjectType::BoolObj(true);
const FALSE: ObjectType = ObjectType::BoolObj(false);
const NULL: ObjectType = ObjectType::NullObj;

pub struct VM<'a> {
    constants: &'a mut Vec<ObjectType>,
    instructions: code::Instructions,
    stack: [ObjectType; STACK_SIZE],
    globals: &'a mut [ObjectType; GLOBAL_SIZE],
    // stack pointer
    sp: usize,
}

impl<'a> VM<'a> {
    pub fn new(compiler: Compiler<'a, '_>, globals: &'a mut [ObjectType; GLOBAL_SIZE]) -> Self {
        VM {
            constants: compiler.constants,
            instructions: compiler.instructions,
            stack: [const { ObjectType::NullObj }; STACK_SIZE],
            globals,
            sp: 0,
        }
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
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
                code::OP_TRUE => self.push(TRUE)?,
                code::OP_FALSE => self.push(FALSE)?,
                code::OP_EQUAL | code::OP_NOT_EQUAL | code::OP_GREATER_THAN => {
                    self.execute_comparison(&op)?
                }
                code::OP_BANG => self.execute_bang_operator()?,
                code::OP_MINUS => self.execute_minus_operator()?,
                code::OP_JUMP => {
                    let pos = code::read_u16(&self.instructions[ip + 1..]);
                    ip = pos as usize - 1;
                }
                code::OP_JUMP_NOT_TRUTHY => {
                    let pos = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;

                    if !Self::is_truthy(self.pop()) {
                        ip = pos as usize - 1;
                    }
                }
                code::OP_NULL => self.push(NULL)?,
                code::OP_SET_GLOBAL => {
                    let global_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.globals[global_index as usize] = self.pop();
                }
                code::OP_GET_GLOBAL => {
                    let global_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.push(self.globals[global_index as usize].clone())?;
                }
                _ => todo!(),
            }

            ip += 1;
        }

        Ok(())
    }

    fn is_truthy(obj: ObjectType) -> bool {
        if let ObjectType::BoolObj(value) = obj {
            value
        } else {
            !matches!(obj, ObjectType::NullObj)
        }
    }

    fn execute_minus_operator(&mut self) -> anyhow::Result<()> {
        let operand = self.pop();

        if let ObjectType::IntegerObj(value) = operand {
            self.push(ObjectType::IntegerObj(-value))
        } else {
            anyhow::bail!("Unsupported type for negation: {}", operand)
        }
    }

    fn execute_bang_operator(&mut self) -> anyhow::Result<()> {
        let operand = self.pop();

        match operand {
            ObjectType::BoolObj(b) => self.push(ObjectType::BoolObj(!b)),
            ObjectType::NullObj => self.push(ObjectType::BoolObj(true)),
            _ => self.push(ObjectType::BoolObj(false)),
        }
    }

    fn execute_comparison(&mut self, op: &Opcode) -> anyhow::Result<()> {
        let right = self.pop();
        let left = self.pop();

        if let ObjectType::IntegerObj(right) = right {
            if let ObjectType::IntegerObj(left) = left {
                return self.execute_int_comparison(op, left, right);
            }
        }

        match *op {
            code::OP_EQUAL => self.push(ObjectType::BoolObj(right == left)),
            code::OP_NOT_EQUAL => self.push(ObjectType::BoolObj(right != left)),
            _ => anyhow::bail!("Unknown operator: {}", op),
        }
    }

    fn execute_int_comparison(&mut self, op: &Opcode, left: f64, right: f64) -> anyhow::Result<()> {
        match *op {
            code::OP_GREATER_THAN => self.push(ObjectType::BoolObj(left > right)),
            code::OP_EQUAL => self.push(ObjectType::BoolObj(left == right)),
            code::OP_NOT_EQUAL => self.push(ObjectType::BoolObj(left != right)),
            _ => anyhow::bail!("Unknown operator: {}", op),
        }
    }

    fn execute_binary_operation(&mut self, op: &Opcode) -> anyhow::Result<()> {
        let right = self.pop();
        let left = self.pop();

        match left {
            ObjectType::IntegerObj(left) => match right {
                ObjectType::IntegerObj(right) => {
                    self.execute_binary_int_operation(op, left, right)?;
                }
                _ => todo!(),
            },
            ObjectType::StringObj(left) => match right {
                ObjectType::StringObj(right) => {
                    self.execute_string_operation(op, left, right)?;
                }
                _ => todo!(),
            },
            _ => todo!(),
        }

        Ok(())
    }

    fn execute_string_operation(
        &mut self,
        op: &Opcode,
        mut left: String,
        right: String,
    ) -> anyhow::Result<()> {
        match *op {
            code::OP_ADD => {
                left.push_str(&right);
                self.push(ObjectType::StringObj(left))
            }
            _ => anyhow::bail!("Unsupported string operator: {}", op),
        }
    }

    fn execute_binary_int_operation(
        &mut self,
        op: &Opcode,
        left: f64,
        right: f64,
    ) -> anyhow::Result<()> {
        match *op {
            code::OP_ADD => self.push(ObjectType::IntegerObj(left + right)),
            code::OP_SUB => self.push(ObjectType::IntegerObj(left - right)),
            code::OP_MUL => self.push(ObjectType::IntegerObj(left * right)),
            code::OP_DIV => self.push(ObjectType::IntegerObj(left / right)),

            _ => anyhow::bail!("Unsupported integer operator: {}", op),
        }
    }

    fn push(&mut self, o: ObjectType) -> anyhow::Result<()> {
        if self.sp >= STACK_SIZE {
            anyhow::bail!("stack overflow".to_string());
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
        compiler::{symbol_table::SymbolTable, Compiler},
        lexer::Lexer,
        object::{self, ObjectType},
        parser::Parser,
        test_setup,
    };
    use core::panic;
    use std::any::Any;

    struct VmTestCase {
        input: &'static str,
        expected: Box<dyn Any>,
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for test in tests {
            let program = test_setup!(&test.input);
            let mut constants = Vec::new();
            let mut symbol_table = SymbolTable::new();
            let mut comp = Compiler::new(&mut constants, &mut symbol_table);
            let mut globals = [const { ObjectType::NullObj }; GLOBAL_SIZE];

            comp.compile(program).unwrap();

            let mut vm = VM::new(comp, &mut globals);
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
        if expected.is::<bool>() {
            test_bool_object(*expected.downcast::<bool>().unwrap(), actual);
            return;
        }
        if expected.is::<&'static str>() {
            test_string_object(*expected.downcast::<&'static str>().unwrap(), actual);
            return;
        }

        // Special null case, probably should be last
        if expected.is::<ObjectType>() {
            if *actual != NULL {
                panic!("object is not null: {expected:?}");
            }
            return;
        }

        panic!("expected f64");
    }

    fn test_string_object(expected: &str, actual: &ObjectType) {
        match actual {
            ObjectType::StringObj(s) => assert_eq!(expected, *s),
            _ => panic!("expected a string object"),
        }
    }

    fn test_bool_object(expected: bool, actual: &object::ObjectType) {
        match actual {
            ObjectType::BoolObj(b) => assert_eq!(expected, *b),
            _ => panic!("expected only bool objects"),
        }
    }

    fn test_integer_object(expected: f64, actual: &object::ObjectType) {
        match actual {
            ObjectType::IntegerObj(x) => assert_eq!(expected, *x),
            _ => panic!("expected only integer objects"),
        }
    }

    macro_rules! vm_test_case {
        ($input:expr, $expected:expr) => {{
            VmTestCase {
                input: $input,
                expected: Box::new($expected),
            }
        }};
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VmTestCase> = vec![
            vm_test_case!("1", 1.0f64),
            vm_test_case!("2", 2.0f64),
            vm_test_case!("1 + 2", 3.0f64),
            vm_test_case!("1 - 2", -1.0f64),
            vm_test_case!("1 * 2", 2.0f64),
            vm_test_case!("2 / 1", 2.0f64),
            vm_test_case!("5 * (2 + 10)", 60.0f64),
            vm_test_case!("-5", -5.0f64),
            vm_test_case!("-10", -10.0f64),
            vm_test_case!("-50 + 100 + -50", 0.0f64),
            vm_test_case!("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50.0f64),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_bool_expressions() {
        let tests = vec![
            vm_test_case!("true", true),
            vm_test_case!("false", false),
            vm_test_case!("1 < 2", true),
            vm_test_case!("1 > 2", false),
            vm_test_case!("1 < 1", false),
            vm_test_case!("1 > 1", false),
            vm_test_case!("1 == 1", true),
            vm_test_case!("1 != 1", false),
            vm_test_case!("1 == 2", false),
            vm_test_case!("1 != 2", true),
            vm_test_case!("true == true", true),
            vm_test_case!("false == false", true),
            vm_test_case!("true == false", false),
            vm_test_case!("true != false", true),
            vm_test_case!("false != true", true),
            vm_test_case!("(1 < 2) == true", true),
            vm_test_case!("(1 < 2) == false", false),
            vm_test_case!("(1 > 2) == true", false),
            vm_test_case!("!true", false),
            vm_test_case!("!false", true),
            vm_test_case!("!5", false),
            vm_test_case!("!!true", true),
            vm_test_case!("!!false", false),
            vm_test_case!("!!5", true),
            vm_test_case!("!(if (false) { 5; })", true),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditional() {
        let tests = vec![
            vm_test_case!("if (true) { 10 }", 10.0f64),
            vm_test_case!("if (true) { 10 } else { 20 }", 10.0f64),
            vm_test_case!("if (false) { 10 } else { 20 }", 20.0f64),
            vm_test_case!("if (1) { 10 }", 10.0f64),
            vm_test_case!("if (1 < 2) { 10 }", 10.0f64),
            vm_test_case!("if (1 < 2) { 10 } else { 20 }", 10.0f64),
            vm_test_case!("if (1 > 2) { 10 } else { 20 }", 20.0f64),
            vm_test_case!("if (1 > 2) { 10 }", NULL),
            vm_test_case!("if (false) { 10 }", NULL),
            vm_test_case!("if ((if (false) { 10 })) { 10 } else { 20 }", 20.0f64),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        run_vm_tests(vec![
            vm_test_case!("let one = 1; one", 1.0f64),
            vm_test_case!("let one = 1; let two = 2; one + two", 3.0f64),
            vm_test_case!("let one = 1; let two = one + one; one + two", 3.0f64),
        ]);
    }

    #[test]
    fn test_string_expressions() {
        run_vm_tests(vec![
            vm_test_case!(r#""monkey""#, "monkey"),
            vm_test_case!(r#""mon" + "key""#, "monkey"),
            vm_test_case!(r#""mon" + "key" + "banana""#, "monkeybanana"),
        ]);
    }
}
