mod frame;

use crate::{
    code::{self, Op},
    compiler::Compiler,
    object::{BuiltinFn, HashPair, ObjectType, BUILTINS},
};
use anyhow::{anyhow, bail};
use frame::Frame;
use std::collections::HashMap;

const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 1000; // TODO: look into why this can't be 65536
const FRAME_SIZE: usize = 1024;
const TRUE: ObjectType = ObjectType::BoolObj(true);
const FALSE: ObjectType = ObjectType::BoolObj(false);
const NULL: ObjectType = ObjectType::NullObj;

pub struct VM<'a> {
    constants: &'a mut Vec<ObjectType>,
    stack: [ObjectType; STACK_SIZE],
    globals: &'a mut [ObjectType; GLOBAL_SIZE],
    // stack pointer
    sp: usize,
    frames: [Frame; FRAME_SIZE],
    frames_index: usize,
}

impl<'a> VM<'a> {
    pub fn new(mut compiler: Compiler<'a>, globals: &'a mut [ObjectType; GLOBAL_SIZE]) -> Self {
        let main_func = ObjectType::CompileFunction(compiler.bytecode().instructions, 0, 0);
        let main_closure = ObjectType::Closure(Box::new(main_func), vec![]);
        let main_frame = Frame::new(main_closure, 0);

        let mut frames = [const { Frame::default() }; FRAME_SIZE];
        frames[0] = main_frame;

        VM {
            constants: compiler.constants,
            stack: [const { ObjectType::NullObj }; STACK_SIZE],
            globals,
            sp: 0,
            frames,
            frames_index: 1,
        }
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
        // ip = 'instruction pointer'
        let mut ip;
        while self.current_frame().ip < self.current_frame().instructions().len() as isize - 1 {
            self.current_frame().ip += 1;

            ip = self.current_frame().ip as usize;
            let instructions = self.current_frame().instructions();
            let op: Op = instructions[ip].into();

            match op {
                Op::Constant => {
                    let const_index = code::read_u16(&instructions[ip + 1..]);
                    self.current_frame().ip += 2;

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
                    let pos = code::read_u16(&instructions[ip + 1..]);
                    self.current_frame().ip = pos as isize - 1;
                }
                Op::JumpNotTruthy => {
                    let pos = code::read_u16(&instructions[ip + 1..]);
                    self.current_frame().ip += 2;

                    if !Self::is_truthy(self.pop()) {
                        self.current_frame().ip = pos as isize - 1;
                    }
                }
                Op::Null => self.push(NULL)?,
                Op::SetGlobal => {
                    let global_index = code::read_u16(&instructions[ip + 1..]);
                    self.current_frame().ip += 2;
                    self.globals[global_index as usize] = self.pop();
                }
                Op::GetGlobal => {
                    let global_index = code::read_u16(&instructions[ip + 1..]);
                    self.current_frame().ip += 2;
                    self.push(self.globals[global_index as usize].clone())?;
                }
                Op::Array => {
                    let num_elements = code::read_u16(&instructions[ip + 1..]);
                    self.current_frame().ip += 2;

                    let array = self.build_array(num_elements);
                    self.sp -= num_elements as usize;

                    self.push(array)?;
                }
                Op::Hash => {
                    let num_elements = code::read_u16(&instructions[ip + 1..]);
                    self.current_frame().ip += 2;

                    let hash = self.build_hash(num_elements)?;
                    self.sp -= num_elements as usize;

                    self.push(hash)?;
                }
                Op::Index => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(left, index)?;
                }
                Op::Call => {
                    let num_args = code::read_u8(&instructions[ip + 1..]);
                    self.current_frame().ip += 1;

                    self.execute_call(num_args.into())?;
                }
                Op::ReturnValue => {
                    // Gets the functions return value from the stack
                    let return_value = self.pop();

                    // Pops the current frame, to the parent frame
                    let frame = self.pop_frame();
                    // Subtracting one also 'pops' the current function from the stack
                    self.sp = frame.base_pointer - 1;

                    // Pushes the returned value from the function onto the stack
                    self.push(return_value)?;
                }
                Op::Return => {
                    let frame = self.pop_frame();
                    // Subtracting one also 'pops' the current function from the stack
                    self.sp = frame.base_pointer - 1;

                    self.push(NULL)?;
                }
                Op::SetLocal => {
                    let local_index = code::read_u8(&instructions[ip + 1..]);
                    self.current_frame().ip += 1;

                    let bp = self.current_frame().base_pointer;
                    let popped = self.pop();
                    self.stack[bp + local_index as usize] = popped;
                }
                Op::GetLocal => {
                    let local_index = code::read_u8(&instructions[ip + 1..]);
                    self.current_frame().ip += 1;

                    let bp = self.current_frame().base_pointer;

                    self.push(self.stack[bp + local_index as usize].clone())?;
                }
                Op::GetBuiltin => {
                    let builtin_index = code::read_u8(&instructions[ip + 1..]);
                    self.current_frame().ip += 1;

                    let definition = &BUILTINS[builtin_index as usize];

                    self.push(ObjectType::BuiltinFunction(definition.builtin))?;
                }
                Op::Closure => {
                    let const_index = code::read_u16(&instructions[ip + 1..]);
                    let num_free = code::read_u8(&instructions[ip + 3..]);
                    self.current_frame().ip += 3;

                    self.push_closure(const_index as usize, num_free as usize)?;
                }
                Op::GetFree => {
                    let free_index = code::read_u8(&instructions[ip + 1..]);
                    self.current_frame().ip += 1;

                    let current_closure = self.current_frame().cl.clone();
                    if let ObjectType::Closure(_, free) = current_closure {
                        self.push(free[free_index as usize].clone())?;
                    }
                }
                Op::CurrentClosure => {
                    let current_closure = self.current_frame().cl.clone();
                    self.push(current_closure)?;
                }
            }
        }

        Ok(())
    }

    fn push_closure(&mut self, const_index: usize, num_free: usize) -> anyhow::Result<()> {
        let constant = &self.constants[const_index];
        if let ObjectType::CompileFunction(_, _, _) = constant {
            let mut free = vec![NULL; num_free];
            for (i, obj) in free.iter_mut().enumerate().take(num_free) {
                *obj = self.stack[self.sp - num_free + i].clone();
            }
            self.sp -= num_free;

            self.push(ObjectType::Closure(Box::new(constant.clone()), free))
        } else {
            bail!("not a function: {:?}", constant)
        }
    }

    fn execute_call(&mut self, num_args: usize) -> anyhow::Result<()> {
        // TODO: get rid of this clone and pass frames by reference
        let callee = self.stack[self.sp - 1 - num_args].clone();

        match callee {
            ObjectType::Closure(_, _) => self.call_closure(callee, num_args),
            ObjectType::BuiltinFunction(callee) => self.call_builtin(&callee, num_args),
            _ => bail!("calling non-function and non-built-in, {:?}", callee),
        }
    }

    fn call_builtin(&mut self, callee: &BuiltinFn, num_args: usize) -> anyhow::Result<()> {
        let args = &self.stack[self.sp - num_args..self.sp];
        let result = callee(args.to_vec());
        self.sp = self.sp - num_args - 1;

        self.push(result)
    }

    fn call_closure(&mut self, callee: ObjectType, num_args: usize) -> anyhow::Result<()> {
        if let ObjectType::Closure(ref func, _) = callee {
            if let ObjectType::CompileFunction(_, num_locals, num_params) = func.as_ref() {
                if num_args != *num_params {
                    bail!("wrong number of arguments: want={num_params}; got={num_args}");
                }

                let frame = Frame::new(callee.clone(), self.sp - num_args);

                let Frame {
                    base_pointer: bp, ..
                } = frame;

                self.push_frame(frame);
                self.sp = bp + num_locals;

                Ok(())
            } else {
                unreachable!("only compiled functions are in closures")
            }
        } else {
            todo!()
        }
    }

    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frames_index] = frame;
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> &Frame {
        self.frames_index -= 1;

        &self.frames[self.frames_index]
    }

    fn execute_index_expression(
        &mut self,
        left: ObjectType,
        index: ObjectType,
    ) -> anyhow::Result<()> {
        match left {
            ObjectType::ArrayObj(array) => {
                if let ObjectType::IntegerObj(int) = index {
                    self.execute_array_index(array, int)
                } else {
                    bail!("index operator not supported: {:?}", index)
                }
            }
            ObjectType::HashObj(hash) => self.execute_hash_index(hash, index),
            _ => bail!("index operator not supported: {:?}", index),
        }
    }

    fn execute_hash_index(
        &mut self,
        hash: HashMap<u64, HashPair>,
        index: ObjectType,
    ) -> anyhow::Result<()> {
        let key = index.hash().map_err(|err| anyhow!(err))?;

        match hash.get(&key) {
            Some(pair) => self.push(pair.value.clone()),
            None => self.push(NULL),
        }
    }

    fn execute_array_index(&mut self, array: Vec<ObjectType>, index: f64) -> anyhow::Result<()> {
        if index < 0.0 || array.is_empty() {
            return self.push(NULL);
        }

        let index = index as usize;

        if index > array.len() - 1 {
            self.push(NULL)
        } else {
            self.push(array.get(index).unwrap_or(&NULL).clone())
        }
    }

    fn build_hash(&mut self, num_elements: u16) -> anyhow::Result<ObjectType> {
        let start_index = self.sp - num_elements as usize;
        let end_index = self.sp;

        let mut hashed_pairs = HashMap::new();

        for i in (start_index..end_index).step_by(2) {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            let pair = HashPair {
                key: key.clone(),
                value,
            };

            let hash_key = key.hash().map_err(|_| anyhow!("unusable hash key"))?;

            hashed_pairs.insert(hash_key, pair);
        }

        Ok(ObjectType::HashObj(hashed_pairs))
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
    use std::{any::Any, collections::HashMap};

    struct VmTestCase {
        input: &'static str,
        expected: Box<dyn Any>,
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for test in tests {
            let program = test_setup!(&test.input);
            let mut constants = Vec::new();
            let symbol_table = SymbolTable::new();
            let mut comp = Compiler::new(&mut constants, symbol_table);
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
            return test_integer_object(*expected.downcast::<f64>().unwrap(), actual);
        }
        if expected.is::<bool>() {
            return test_bool_object(*expected.downcast::<bool>().unwrap(), actual);
        }
        if expected.is::<&'static str>() {
            return test_string_object(*expected.downcast::<&'static str>().unwrap(), actual);
        }
        if expected.is::<Vec<f64>>() {
            return test_array_object(*expected.downcast::<Vec<f64>>().unwrap(), actual);
        }
        if expected.is::<HashMap<u64, f64>>() {
            return test_hash_object(*expected.downcast::<HashMap<u64, f64>>().unwrap(), actual);
        }

        if expected.is::<ObjectType>() {
            return test_object_type(*expected.downcast::<ObjectType>().unwrap(), actual);
        }
        // Special null case, probably should be last
        if expected.is::<ObjectType>() {
            if *actual != NULL {
                panic!("object is not null: {expected:?}");
            }
            return;
        }

        todo!("type not yet ready for testing");
    }

    fn test_object_type(expected: ObjectType, actual: &ObjectType) {
        match expected {
            ObjectType::NullObj => assert_eq!(expected, *actual),
            ObjectType::ErrorObj(s) => {
                if let ObjectType::ErrorObj(actual_s) = actual {
                    assert_eq!(s, *actual_s);
                }
            }
            _ => panic!("object type not handled: {:?}", expected),
        }
    }

    fn test_hash_object(expected: HashMap<u64, f64>, actual: &ObjectType) {
        match actual {
            ObjectType::HashObj(hash) => {
                assert_eq!(hash.len(), expected.len());
                for (expected_key, expected_value) in expected.iter() {
                    test_integer_object(*expected_value, &hash.get(expected_key).unwrap().value);
                }
            }
            _ => panic!("expected a hash object, got: {:?}", actual),
        }
    }

    fn test_array_object(expected: Vec<f64>, actual: &ObjectType) {
        match actual {
            ObjectType::ArrayObj(objs) => {
                assert_eq!(expected.len(), objs.len());
                for (i, obj) in objs.iter().enumerate() {
                    test_integer_object(expected[i], obj);
                }
            }
            _ => panic!("expected an array object, got: {:?}", actual),
        }
    }

    fn test_string_object(expected: &str, actual: &ObjectType) {
        match actual {
            ObjectType::StringObj(s) => assert_eq!(expected, *s),
            _ => panic!("expected a string object, got: {:?}", actual),
        }
    }

    fn test_bool_object(expected: bool, actual: &object::ObjectType) {
        match actual {
            ObjectType::BoolObj(b) => assert_eq!(expected, *b),
            _ => panic!("expected only bool objects, got: {:?}", actual),
        }
    }

    fn test_integer_object(expected: f64, actual: &object::ObjectType) {
        match actual {
            ObjectType::IntegerObj(x) => assert_eq!(expected, *x),
            _ => panic!("expected only integer objects, got: {:?}", actual),
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

    #[test]
    fn test_hash_literals() {
        let mut hash1 = HashMap::new();
        hash1.insert(ObjectType::IntegerObj(1.0).hash().unwrap(), 2.0f64);
        hash1.insert(ObjectType::IntegerObj(2.0).hash().unwrap(), 3.0f64);

        let mut hash2 = HashMap::new();
        hash2.insert(ObjectType::IntegerObj(2.0).hash().unwrap(), 4.0f64);
        hash2.insert(ObjectType::IntegerObj(6.0).hash().unwrap(), 16.0f64);

        run_vm_tests(vec![
            vm_test_case!("{1: 2, 2: 3}", hash1),
            vm_test_case!("{1 + 1: 2 * 2, 3 + 3: 4 * 4}", hash2),
        ]);
    }

    #[test]
    fn test_index_expressions() {
        run_vm_tests(vec![
            vm_test_case!("[1, 2, 3][1]", 2f64),
            vm_test_case!("[1,2,3][0 + 2]", 3f64),
            vm_test_case!("[[1,1,1]][0][0]", 1f64),
            vm_test_case!("[][0]", NULL),
            vm_test_case!("[1,2,3][99]", NULL),
            vm_test_case!("[1][-1]", NULL),
            vm_test_case!("{1: 1, 2: 2}[1]", 1f64),
            vm_test_case!("{1: 1, 2: 2}[2]", 2f64),
            vm_test_case!("{1: 1}[0]", NULL),
            vm_test_case!("{}[0]", NULL),
        ]);
    }

    #[test]
    fn test_function_calls() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let fivePlusTen = fn() { 5 + 10 };
                    fivePlusTen();
                "#,
                15f64
            ),
            vm_test_case!(
                r#"
                    let one = fn() { 1; };
                    let two = fn() { 2; };
                    one() + two()
                "#,
                3f64
            ),
            vm_test_case!(
                r#"
                    let a = fn() { 1 };
                    let b = fn() { a() + 1 };
                    let c = fn() { b() + 1 };
                    c();
                "#,
                3f64
            ),
        ]);
    }

    #[test]
    fn test_functions_with_return_statements() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let earlyExit = fn() { return 99; 100; };
                    earlyExit();
                "#,
                99f64
            ),
            vm_test_case!(
                r#"
                    let earlyExit = fn() { return 99; 100; };
                    earlyExit();
                "#,
                99f64
            ),
        ]);
    }

    #[test]
    fn test_functions_without_return_values() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let noReturn = fn() { };
                    noReturn();
                "#,
                NULL
            ),
            vm_test_case!(
                r#"
                    let noReturn = fn() { };
                    let noReturnTwo = fn() { };
                    noReturn();
                    noReturnTwo();
                "#,
                NULL
            ),
        ]);
    }

    #[test]
    fn test_first_class_functions() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let returnsOne = fn() { 1; };
                    let returnsOneReturner = fn() { returnsOne; };
                    returnsOneReturner()();
                "#,
                1f64
            ),
            vm_test_case!(
                r#"
                    let returnsOneReturner = fn() {
                        let returnsOne = fn() {
                            1;
                        };
                        returnsOne;
                    };
                    returnsOneReturner()();
                "#,
                1f64
            ),
        ]);
    }

    #[test]
    fn test_calling_funcs_with_bindings() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let one = fn() { let one = 1; one };
                    one();
                "#,
                1f64
            ),
            vm_test_case!(
                r#"
                    let oneAndTwo = fn() { let one = 1; let two = 2; one + two };
                    oneAndTwo();
                "#,
                3f64
            ),
            vm_test_case!(
                r#"
                    let oneAndTwo = fn() { let one = 1; let two = 2; one + two };
                    let threeAndFour = fn() { let three = 3; let four = 4; three + four };
                    oneAndTwo() + threeAndFour();
                "#,
                10f64
            ),
            vm_test_case!(
                r#"
                    let firstFoobar = fn() { let foobar = 50; foobar; };
                    let secondFoobar = fn() { let foobar = 100; foobar; };
                    firstFoobar() + secondFoobar();
                "#,
                150f64
            ),
            vm_test_case!(
                r#"
                    let globalSeed = 50;
                    let minusOne = fn() { 
                        let num = 1;
                        globalSeed - num;
                    };
                    let minusTwo = fn() { 
                        let num = 2;
                        globalSeed - num;
                    };
                    minusOne() + minusTwo();
                "#,
                97f64
            ),
        ]);
    }

    #[test]
    fn test_calling_funcs_with_args_and_bindings() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let identity = fn(a) { a; };
                    identity(4);
                "#,
                4f64
            ),
            vm_test_case!(
                r#"
                    let sum = fn(a, b) { a + b; };
                    sum(1, 2);
                "#,
                3f64
            ),
            vm_test_case!(
                r#"
                    let sum = fn(a, b) {
                        let c = a + b;
                        c;
                    };

                    sum(1, 2);
                "#,
                3f64
            ),
            vm_test_case!(
                r#"
                    let sum = fn(a, b) {
                        let c = a + b;
                        c;
                    };

                    sum(1, 2) + sum(3, 4);
                "#,
                10f64
            ),
            vm_test_case!(
                r#"
                    let sum = fn(a, b) {
                        let c = a + b;
                        c;
                    };

                    let outer = fn() {
                        sum(1, 2) + sum(3, 4);
                    };

                    outer();
                "#,
                10f64
            ),
            vm_test_case!(
                r#"
                    let globalNum = 10;

                    let sum = fn(a, b) {
                        let c = a + b;
                        c + globalNum;
                    };

                    let outer = fn() {
                        sum(1, 2) + sum(3, 4) + globalNum;
                    };

                    outer() + globalNum;
                "#,
                50f64
            ),
        ]);
    }

    #[test]
    fn call_functions_with_wrong_arguments() {
        let tests = vec![
            vm_test_case!(
                r#"
                    fn() { 1; }(1);
                "#,
                "wrong number of arguments: want=0; got=1"
            ),
            vm_test_case!(
                r#"
                    fn(a) { a; }();
                "#,
                "wrong number of arguments: want=1; got=0"
            ),
            vm_test_case!(
                r#"
                    fn(a, b) { a + b; }(1);
                "#,
                "wrong number of arguments: want=2; got=1"
            ),
        ];

        for test in tests {
            let program = test_setup!(&test.input);
            let mut constants = Vec::new();
            let symbol_table = SymbolTable::new();
            let mut comp = Compiler::new(&mut constants, symbol_table);
            let mut globals = [const { ObjectType::NullObj }; GLOBAL_SIZE];

            comp.compile(program).unwrap();

            let mut vm = VM::new(comp, &mut globals);

            let expected_error_msg = test.expected.downcast::<&'static str>().unwrap();

            match vm.run() {
                Ok(_) => panic!("Expected vm to fail"),
                Err(msg) => assert_eq!(msg.to_string(), *expected_error_msg),
            }
        }
    }

    #[test]
    fn test_builtin_funcs() {
        run_vm_tests(vec![
            vm_test_case!("len(\"\")", 0f64),
            vm_test_case!("len(\"four\")", 4f64),
            vm_test_case!("len(\"hello world\")", 11f64),
            vm_test_case!(
                "len(1)",
                ObjectType::ErrorObj("argument to `len` not supported, got INTEGER".into())
            ),
            vm_test_case!(
                "len(\"one\", \"two\")",
                ObjectType::ErrorObj("wrong number of arguments. got=2, want=1".into())
            ),
            vm_test_case!("len([1,2,3])", 3f64),
            vm_test_case!("len([])", 0f64),
            vm_test_case!("puts(\"hello\", \"world\")", NULL),
            vm_test_case!("first([1,2,3])", 1f64),
            vm_test_case!("first([])", NULL),
            vm_test_case!(
                "first(1)",
                ObjectType::ErrorObj("argument to `first` must be ARRAY, got INTEGER".into())
            ),
            vm_test_case!("last([1,2,3])", 3f64),
            vm_test_case!("last([])", NULL),
            vm_test_case!(
                "last(1)",
                ObjectType::ErrorObj("argument to `last` must be ARRAY, got INTEGER".into())
            ),
            vm_test_case!("rest([1,2,3])", vec![2f64, 3f64]),
            vm_test_case!("rest([])", NULL),
            vm_test_case!("push([], 1)", vec![1f64]),
            vm_test_case!(
                "push(1, 1)",
                ObjectType::ErrorObj("argument to `push` must be ARRAY, got INTEGER".into())
            ),
        ]);
    }

    #[test]
    fn test_closures() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let newClosure = fn(a) {
                        fn() { a };
                    };
                    let closure = newClosure(99);
                    closure();
                "#,
                99.0
            ),
            vm_test_case!(
                r#"
                    let newAdder = fn(a, b) {
                        fn(c) { a + b + c };
                    };
                    let adder = newAdder(1,2);
                    adder(8);
                "#,
                11.0
            ),
            vm_test_case!(
                r#"
                    let newAdder = fn(a, b) {
                        let c = a + b;
                        fn(d) { c + d };
                    };
                    let adder = newAdder(1, 2);
                    adder(8);
                "#,
                11.0
            ),
            vm_test_case!(
                r#"
                    let newAdderOuter = fn(a, b) {
                        let c = a + b;
                        fn(d) {
                            let e = d + c;
                            fn(f) { e + f; };
                        };
                    };
                    let newAdderInner = newAdderOuter(1, 2)
                    let adder = newAdderInner(3);
                    adder(8);
                "#,
                14.0
            ),
            vm_test_case!(
                r#"
                    let a = 1;
                    let newAdderOuter = fn(b) {
                        fn(c) {
                            fn(d) { a + b + c + d };
                        };
                    };
                    let newAdderInner = newAdderOuter(2)
                    let adder = newAdderInner(3);
                    adder(8);
            "#,
                14.0
            ),
            vm_test_case!(
                r#"
                    let newClosure = fn(a, b) {
                        let one = fn() { a; };
                        let two = fn() { b; };
                        fn() { one() + two(); };
                    };
                    let closure = newClosure(9, 90);
                    closure();
                "#,
                99.0
            ),
        ]);
    }

    #[test]
    fn test_recursive_functions() {
        run_vm_tests(vec![
            vm_test_case!(
                r#"
                    let countDown = fn(x) {
                        if (x == 0) {
                            return 0;
                        } else {
                            countDown(x - 1);
                        }
                    };
                    countDown(1);
                "#,
                0.0
            ),
            vm_test_case!(
                r#"
                    let countDown = fn(x) {
                        if (x == 0) {
                            return 0;
                        } else {
                            countDown(x - 1);
                        }
                    };
                    let wrapper = fn() {
                        countDown(1);
                    };
                    wrapper();
                "#,
                0.0
            ),
            vm_test_case!(
                r#"
                    let wrapper = fn() {
                        let countDown = fn(x) {
                            if (x == 0) {
                                return 0;
                            } else {
                                countDown(x - 1);
                            }
                        };
                        countDown(1);
                    };
                    wrapper();
                "#,
                0.0
            ),
        ]);
    }

    #[test]
    fn test_fibonacci() {
        run_vm_tests(vec![vm_test_case!(
            r#"
            let fibonacci = fn(x) {
                if (x == 0) {
                    return 0;
                }
                if (x == 1) {
                    return 1;
                }
                return fibonacci(x - 1) + fibonacci(x - 2);
            };
            fibonacci(15);
        "#,
            610.0
        )]);
    }
}
