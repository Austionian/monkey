use crate::{
    code::{self, Op},
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
            let op: Op = self.instructions[ip].into();

            match op {
                Op::Constant => {
                    let const_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;

                    // TODO: remove this clone and cast
                    self.push(self.constants[const_index as usize].clone())?;
                }
                Op::Add | code::Op::Sub | code::Op::Mul | code::Op::Div => {
                    self.execute_binary_operation(&op)?
                }
                Op::Pop => {
                    self.pop();
                }
                Op::True => self.push(TRUE)?,
                Op::False => self.push(FALSE)?,
                Op::Equal | Op::NotEqual | Op::GreaterThan => self.execute_comparison(&op)?,
                Op::Bang => self.execute_bang_operator()?,
                Op::Minus => self.execute_minus_operator()?,
                Op::Jump => {
                    let pos = code::read_u16(&self.instructions[ip + 1..]);
                    ip = pos as usize - 1;
                }
                Op::JumpNotTruthy => {
                    let pos = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;

                    if !Self::is_truthy(self.pop()) {
                        ip = pos as usize - 1;
                    }
                }
                Op::Null => self.push(NULL)?,
                Op::SetGlobal => {
                    let global_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.globals[global_index as usize] = self.pop();
                }
                Op::GetGlobal => {
                    let global_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.push(self.globals[global_index as usize].clone())?;
                }
                Op::Array => {
                    let num_elements = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;

                    let array = self.build_array(num_elements);
                    self.sp -= num_elements as usize;

                    self.push(array)?;
                }
                Op::Hash => {
                    todo!()
                }
            }

            ip += 1;
        }

        Ok(())
    }

    fn build_array(&mut self, num_elements: u16) -> ObjectType {
        let start_index = self.sp - num_elements as usize;
        let end_index = self.sp;
        let mut elements = vec![ObjectType::NullObj; end_index - start_index];

        elements[..(end_index - start_index)].clone_from_slice(&self.stack[start_index..end_index]);

        ObjectType::ArrayObj(elements)
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

    fn execute_comparison(&mut self, op: &Op) -> anyhow::Result<()> {
        let right = self.pop();
        let left = self.pop();

        if let ObjectType::IntegerObj(right) = right {
            if let ObjectType::IntegerObj(left) = left {
                return self.execute_int_comparison(op, left, right);
            }
        }

        match *op {
            Op::Equal => self.push(ObjectType::BoolObj(right == left)),
            Op::NotEqual => self.push(ObjectType::BoolObj(right != left)),
            _ => anyhow::bail!("Unknown operator: {}", op),
        }
    }

    fn execute_int_comparison(&mut self, op: &Op, left: f64, right: f64) -> anyhow::Result<()> {
        match *op {
            Op::GreaterThan => self.push(ObjectType::BoolObj(left > right)),
            Op::Equal => self.push(ObjectType::BoolObj(left == right)),
            Op::NotEqual => self.push(ObjectType::BoolObj(left != right)),
            _ => anyhow::bail!("Unknown operator: {}", op),
        }
    }

    fn execute_binary_operation(&mut self, op: &Op) -> anyhow::Result<()> {
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
        op: &Op,
        mut left: String,
        right: String,
    ) -> anyhow::Result<()> {
        match *op {
            Op::Add => {
                left.push_str(&right);
                self.push(ObjectType::StringObj(left))
            }
            _ => anyhow::bail!("Unsupported string operator: {}", op),
        }
    }

    fn execute_binary_int_operation(
        &mut self,
        op: &Op,
        left: f64,
        right: f64,
    ) -> anyhow::Result<()> {
        match *op {
            Op::Add => self.push(ObjectType::IntegerObj(left + right)),
            Op::Sub => self.push(ObjectType::IntegerObj(left - right)),
            Op::Mul => self.push(ObjectType::IntegerObj(left * right)),
            Op::Div => self.push(ObjectType::IntegerObj(left / right)),

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
        if expected.is::<Vec<f64>>() {
            test_array_object(*expected.downcast::<Vec<f64>>().unwrap(), actual);
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

    fn test_array_object(expected: Vec<f64>, actual: &ObjectType) {
        match actual {
            ObjectType::ArrayObj(objs) => {
                assert_eq!(expected.len(), objs.len());
                for (i, obj) in objs.iter().enumerate() {
                    test_integer_object(expected[i], obj);
                }
            }
            _ => panic!("expected an array object"),
        }
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
        run_vm_tests(vec![
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
        ]);
    }

    #[test]
    fn test_bool_expressions() {
        run_vm_tests(vec![
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
        ]);
    }

    #[test]
    fn test_conditional() {
        run_vm_tests(vec![
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
        ]);
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

    #[test]
    fn test_array_literals() {
        run_vm_tests(vec![
            vm_test_case!("[]", Vec::<f64>::new()),
            vm_test_case!("[1, 2, 3]", vec![1.0f64, 2.0f64, 3.0f64]),
            vm_test_case!("[1 + 2, 3 - 4, 5 * 6]", vec![3.0f64, -1.0f64, 30.0f64]),
        ]);
    }
}
