mod frame;

use anyhow::{anyhow, bail};
use code::{self, Op};
use compiler::Compiler;
use frame::Frame;
use object::{BUILTINS, BuiltinFn, HashPair, ObjectType};
use std::collections::HashMap;

pub const GLOBAL_SIZE: usize = 1024;

pub const STACK_SIZE: usize = 2048;
pub const FRAME_SIZE: usize = 1024;
pub const TRUE: ObjectType = ObjectType::BoolObj(true);
pub const FALSE: ObjectType = ObjectType::BoolObj(false);
pub const NULL: ObjectType = ObjectType::NullObj;

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

                    self.push(self.constants[const_index as usize].clone())?;
                }
                Op::Add | Op::Sub | Op::Mul | Op::Div => self.execute_binary_operation(&op)?,
                Op::Pop => {
                    self.pop();
                }
                Op::True => self.push(TRUE)?,
                Op::False => self.push(FALSE)?,
                Op::Equal | Op::NotEqual | Op::GreaterThan | Op::Or | Op::And => {
                    self.execute_comparison(&op)?
                }
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
            bail!("unexpected callee: {}", callee)
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
            bail!("Unsupported type for negation: {}", operand)
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
            Op::Or => self.push(ObjectType::BoolObj(
                right.to_native_bool() || left.to_native_bool(),
            )),
            Op::And => self.push(ObjectType::BoolObj(
                right.to_native_bool() && left.to_native_bool(),
            )),
            _ => bail!("unexpected operator: {}", op),
        }
    }

    fn execute_int_comparison(&mut self, op: &Op, left: f64, right: f64) -> anyhow::Result<()> {
        match *op {
            Op::GreaterThan => self.push(ObjectType::BoolObj(left > right)),
            Op::Equal => self.push(ObjectType::BoolObj(left == right)),
            Op::NotEqual => self.push(ObjectType::BoolObj(left != right)),
            Op::Or => self.push(ObjectType::BoolObj((left != 0.0) || (right != 0.0))),
            Op::And => self.push(ObjectType::BoolObj((left != 0.0) && (right != 0.0))),
            _ => bail!("unexpected operator: {}", op),
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
                _ => bail!("unexpected operation: INTEGER {} {}", op, right),
            },
            ObjectType::StringObj(left) => match right {
                ObjectType::StringObj(right) => {
                    self.execute_string_operation(op, left, right)?;
                }
                _ => bail!("unexpected operation: STRING {} {}", op, right),
            },
            _ => bail!("unexpected operation: {} {} {}", left, op, right),
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
            _ => bail!("unsupported string operator: {}", op),
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

            _ => bail!("Unsupported integer operator: {}", op),
        }
    }

    fn push(&mut self, o: ObjectType) -> anyhow::Result<()> {
        if self.sp >= STACK_SIZE {
            bail!("stack overflow".to_string());
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
