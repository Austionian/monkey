pub mod symbol_table;
use ast::{self, BlockStatement, Expression, LetStatement, ReturnStatement, Statement};
use code::{self, Op, make};
use object::{self, ObjectType};
use symbol_table::{
    BUILTIN_SCOPE, FREE_SCOPE, FUNCTION_SCOPE, GLOBAL_SCOPE, LOCAL_SCOPE, Symbol, SymbolTable,
};
use thiserror::Error;
use token::{Token, TokenLiteral};

pub struct Compiler<'a> {
    pub constants: &'a mut Vec<object::ObjectType>,
    pub symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

pub struct Environment {
    pub constants: Vec<object::ObjectType>,
    pub symbol_table: SymbolTable,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct CompilationScope {
    instructions: code::Instructions,
    // very last instruction
    last_instruction: EmittedInstruction,
    // instruction before last_instruction
    previous_instruction: EmittedInstruction,
}

#[derive(Default, Clone, Debug)]
pub struct EmittedInstruction {
    opcode: Op,
    position: usize,
}

pub struct ByteCode {
    pub instructions: code::Instructions,
}

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("undefined variable")]
    UndefinedVariable,
    #[error("invalid token {0:?}")]
    InvalidToken(Token),
}

pub trait Compile {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), CompilerError>;
}

impl Compile for Expression {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), CompilerError> {
        match self {
            Self::IntExpression(t) => {
                let integer = t.to_owned().into();
                let i = compiler.add_constant(integer);
                let _ = compiler.emit(&code::Op::Constant, vec![i]);
            }
            Self::BoolExpression(t) => match t {
                Token::True => {
                    compiler.emit(&Op::True, vec![]);
                }
                Token::False => {
                    compiler.emit(&Op::False, vec![]);
                }
                _ => unreachable!("Only bools should be in bool expressions"),
            },
            Self::InfixExpression((operator, left, right)) => {
                if operator == &Token::Lt {
                    // Flip the order rather than make an OP_LESS_THAN
                    right.as_ref().compile(compiler)?;
                    left.as_ref().compile(compiler)?;
                    compiler.emit(&Op::GreaterThan, vec![]);

                    return Ok(());
                }
                left.as_ref().compile(compiler)?;
                right.as_ref().compile(compiler)?;

                match operator {
                    Token::Plus => compiler.emit(&Op::Add, vec![]),
                    Token::Minus => compiler.emit(&Op::Sub, vec![]),
                    Token::Slash => compiler.emit(&Op::Div, vec![]),
                    Token::Asterisk => compiler.emit(&Op::Mul, vec![]),
                    Token::Gt => compiler.emit(&Op::GreaterThan, vec![]),
                    Token::Eq => compiler.emit(&Op::Equal, vec![]),
                    Token::Not_eq => compiler.emit(&Op::NotEqual, vec![]),
                    Token::Or => compiler.emit(&Op::Or, vec![]),
                    Token::And => compiler.emit(&Op::And, vec![]),
                    _ => todo!(),
                };
            }
            Self::PrefixExpression((operator, right)) => {
                right.as_ref().compile(compiler)?;

                match operator {
                    Token::Bang => compiler.emit(&Op::Bang, vec![]),
                    Token::Minus => compiler.emit(&Op::Minus, vec![]),
                    _ => todo!(),
                };
            }
            Self::IfExpression(condition, consequence, alternative) => {
                condition.as_ref().compile(compiler)?;

                // emit with bogus jump value
                let jump_not_truthy = compiler.emit(&Op::JumpNotTruthy, vec![9999]);

                consequence.as_ref().compile(compiler)?;

                if compiler.last_instruction_is(&Op::Pop) {
                    compiler.remove_last_pop();
                }

                // emit a jump with a bogus value
                let jump_position = compiler.emit(&Op::Jump, vec![9999]);

                let after_consequence_position = compiler.current_instructions().len();
                compiler.change_operand(jump_not_truthy, after_consequence_position);

                if let Some(alternative) = alternative {
                    alternative.as_ref().compile(compiler)?;

                    if compiler.last_instruction_is(&Op::Pop) {
                        compiler.remove_last_pop();
                    }
                } else {
                    compiler.emit(&Op::Null, vec![]);
                }

                let after_alternative_position = compiler.current_instructions().len();
                compiler.change_operand(jump_position, after_alternative_position);
            }
            Self::IdentExpression(ident) => {
                let name = if let Token::Ident(name) = ident {
                    name
                } else {
                    return Err(CompilerError::InvalidToken(ident.clone()));
                };
                let symbol = compiler
                    .symbol_table
                    .resolve(name)
                    .ok_or(CompilerError::UndefinedVariable)?;

                // why does moving
                // `compiler.load_symbol(symbol);`
                // here break the tests?
                match symbol.scope {
                    GLOBAL_SCOPE => compiler.emit(&Op::GetGlobal, vec![symbol.index]),
                    LOCAL_SCOPE => compiler.emit(&Op::GetLocal, vec![symbol.index]),
                    BUILTIN_SCOPE => compiler.emit(&Op::GetBuiltin, vec![symbol.index]),
                    FREE_SCOPE => compiler.emit(&Op::GetFree, vec![symbol.index]),
                    FUNCTION_SCOPE => compiler.emit(&Op::CurrentClosure, vec![]),
                    _ => unreachable!("there should only be four scopes!"),
                };
            }
            Self::StringExpression(string) => {
                let i = compiler.add_constant(string.to_owned().into());
                compiler.emit(&Op::Constant, vec![i]);
            }
            Self::ArrayExpression(expressions) => {
                for expression in expressions {
                    expression.compile(compiler)?;
                }

                compiler.emit(&Op::Array, vec![expressions.len()]);
            }
            Self::HashLiteral(hash) => {
                // Sort values in test so the tests can be deterministic
                #[cfg(test)]
                {
                    let mut keys = Vec::new();
                    for kv in &hash.pairs {
                        keys.push(kv);
                    }
                    keys.sort_by(|a, b| a.0.to_string().cmp(&b.0.to_string()));

                    for kv in keys {
                        kv.0.compile(compiler)?;
                        kv.1.compile(compiler)?;
                    }
                }

                // No need to sort when it comes to the real deal
                #[cfg(not(test))]
                {
                    for kv in hash.pairs.iter() {
                        kv.0.compile(compiler)?;
                        kv.1.compile(compiler)?;
                    }
                }

                compiler.emit(&Op::Hash, vec![hash.pairs.len() * 2]);
            }
            Self::IndexExpression(left, index) => {
                left.compile(compiler)?;
                index.compile(compiler)?;
                compiler.emit(&Op::Index, vec![]);
            }
            Self::FunctionLiteral(_, params, block, name) => {
                compiler.enter_scope();

                if name.borrow().is_some() {
                    compiler
                        .symbol_table
                        .define_function_name(name.borrow().clone().unwrap());
                }

                for param in params {
                    compiler.symbol_table.define(param.token_literal());
                }

                block.compile(compiler)?;

                if compiler.last_instruction_is(&Op::Pop) {
                    compiler.replace_last_pop_with_return();
                }
                if !compiler.last_instruction_is(&Op::ReturnValue) {
                    compiler.emit(&Op::Return, vec![]);
                }

                let free_symbols = compiler.symbol_table.free_symbols.clone();
                let num_locals = compiler.symbol_table.num_definitions;
                let instructions = compiler.leave_scope();

                for symbol in free_symbols.borrow().iter() {
                    compiler.load_symbol(symbol);
                }

                let compiled_fn =
                    ObjectType::CompileFunction(instructions, num_locals, params.len());

                let constant = compiler.add_constant(compiled_fn);

                compiler.emit(&Op::Closure, vec![constant, free_symbols.borrow().len()]);
            }
            Self::CallExpression(function, args) => {
                function.compile(compiler)?;
                for arg in args {
                    arg.compile(compiler)?;
                }
                compiler.emit(&Op::Call, vec![args.len()]);
            }
            _ => panic!("no done yet {self:?}"),
        }

        Ok(())
    }
}

impl Compile for Statement {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), CompilerError> {
        match self {
            Self::LetStatement(let_statement) => let_statement.compile(compiler),
            Self::ExpressStatement(expression) => {
                expression.compile(compiler)?;
                compiler.emit(&code::Op::Pop, vec![]);

                Ok(())
            }
            Self::BlockStatement(block_statement) => block_statement.compile(compiler),
            Self::ReturnStatement(return_statement) => return_statement.compile(compiler),
        }
    }
}

impl Compile for ReturnStatement {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), CompilerError> {
        self.value.compile(compiler)?;
        compiler.emit(&Op::ReturnValue, vec![]);

        Ok(())
    }
}

impl Compile for LetStatement {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), CompilerError> {
        let name = if let Token::Ident(name) = &self.name {
            name
        } else {
            return Err(CompilerError::InvalidToken(self.name.clone()));
        };

        let symbol = compiler.symbol_table.define(name.to_string());

        self.value.compile(compiler)?;

        if symbol.scope == GLOBAL_SCOPE {
            compiler.emit(&Op::SetGlobal, vec![symbol.index]);
        } else {
            compiler.emit(&Op::SetLocal, vec![symbol.index]);
        }

        Ok(())
    }
}

impl Compile for BlockStatement {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), CompilerError> {
        for statement in self.statements.iter() {
            statement.compile(compiler)?;
        }

        Ok(())
    }
}

impl<'a> Compiler<'a> {
    pub fn new(constants: &'a mut Vec<ObjectType>, mut symbol_table: SymbolTable) -> Self {
        for (i, v) in object::BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i, &v.name);
        }
        Self {
            constants,
            symbol_table,
            scopes: vec![CompilationScope {
                instructions: Vec::new(),
                last_instruction: EmittedInstruction::default(),
                previous_instruction: EmittedInstruction::default(),
            }],
            scope_index: 0,
        }
    }

    pub fn compile(&mut self, node: ast::Program) -> Result<(), CompilerError> {
        for statement in node.statements {
            statement.compile(self)?;
        }

        Ok(())
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: Vec::new(),
            last_instruction: EmittedInstruction::default(),
            previous_instruction: EmittedInstruction::default(),
        };

        self.scopes.push(scope);
        self.scope_index += 1;

        self.symbol_table = SymbolTable::new_enclosed(Box::new(self.symbol_table.clone()));
    }

    fn leave_scope(&mut self) -> Vec<u8> {
        let instructions = self.current_instructions().to_owned();

        self.scopes = self.scopes[..self.scopes.len() - 1].to_vec();
        self.scope_index -= 1;

        self.symbol_table = if let Some(outer) = self.symbol_table.outer.as_ref() {
            *outer.clone()
        } else {
            unreachable!("there should always be an outer when leaving a scope")
        };

        instructions
    }

    fn current_instructions(&mut self) -> &mut code::Instructions {
        &mut self.scopes[self.scope_index].instructions
    }

    fn previous_instruction(&mut self) -> &EmittedInstruction {
        &self.scopes[self.scope_index].previous_instruction
    }

    fn last_instruction(&mut self) -> &EmittedInstruction {
        &self.scopes[self.scope_index].last_instruction
    }

    fn remove_last_pop(&mut self) {
        let last = self.last_instruction().clone();

        let old = self.current_instructions();
        let new = &old[..last.position];

        self.scopes[self.scope_index].instructions = new.to_vec();
        self.scopes[self.scope_index].last_instruction = self.previous_instruction().clone();
    }

    fn last_instruction_is(&mut self, op: &Op) -> bool {
        if self.current_instructions().is_empty() {
            return false;
        }

        &self.scopes[self.scope_index].last_instruction.opcode == op
    }

    pub fn add_constant(&mut self, obj: object::ObjectType) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    pub fn emit(&mut self, op: &Op, operands: Vec<usize>) -> usize {
        let mut ins = make::it!(op, operands);

        let pos = self.add_instruction(&mut ins);

        self.set_last_instruction(op, pos);

        pos
    }

    fn change_operand(&mut self, op_position: usize, operand: usize) {
        let op = self.current_instructions()[op_position];
        let new_instruction = make::it!(&op.into(), vec![operand]);

        self.replace_instruction(op_position, new_instruction);
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: Vec<u8>) {
        let ins = self.current_instructions();

        for i in 0..new_instruction.len() {
            ins[position + i] = new_instruction[i]
        }
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pos = self.scopes[self.scope_index].last_instruction.position;
        self.replace_instruction(last_pos, make::it!(&Op::ReturnValue));

        self.scopes[self.scope_index].last_instruction.opcode = Op::ReturnValue;
    }

    fn set_last_instruction(&mut self, op: &Op, position: usize) {
        let previous = self.last_instruction().clone();
        let last = EmittedInstruction {
            opcode: *op,
            position,
        };

        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = last;
    }

    fn add_instruction(&mut self, ins: &mut Vec<u8>) -> usize {
        let pos_new_instruction = self.current_instructions().len();
        self.current_instructions().append(ins);

        pos_new_instruction
    }

    pub fn load_symbol(&mut self, symbol: &Symbol) {
        match symbol.scope {
            GLOBAL_SCOPE => self.emit(&Op::GetGlobal, vec![symbol.index]),
            LOCAL_SCOPE => self.emit(&Op::GetLocal, vec![symbol.index]),
            BUILTIN_SCOPE => self.emit(&Op::GetBuiltin, vec![symbol.index]),
            FREE_SCOPE => self.emit(&Op::GetFree, vec![symbol.index]),
            FUNCTION_SCOPE => self.emit(&Op::CurrentClosure, vec![]),
            _ => unreachable!("there should only be four scopes!"),
        };
    }

    pub fn bytecode(&mut self) -> ByteCode {
        ByteCode {
            instructions: self.current_instructions().to_vec(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use code::Instructions;
    use core::panic;
    use lexer::Lexer;
    use object::ObjectType;
    use parser::{Parser, test_setup};
    use std::any::Any;

    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<Box<dyn Any>>,
        expected_instructions: Vec<code::Instructions>,
    }

    macro_rules! compiler_test_case {
        ($input:expr, $instructions:expr) => {{
            CompilerTestCase {
                input: $input,
                expected_constants: Vec::<Box<dyn Any>>::new(),
                expected_instructions: $instructions,
            }
        }};

        ($input:expr, $instructions:expr, ($($constant:expr), *)) => {{
            let mut expected_constants = Vec::<Box<dyn Any>>::new();
            $(
                expected_constants.push(Box::new($constant));
            )*
            CompilerTestCase {
                input: $input,
                expected_constants,
                expected_instructions: $instructions,
            }
        }};
    }

    /// helper function
    fn concat_instructions(expected: Vec<code::Instructions>) -> code::Instructions {
        expected.into_iter().flatten().collect()
    }

    fn test_instructions(expected: Vec<code::Instructions>, actual: &code::Instructions) {
        let concatted = concat_instructions(expected);

        assert_eq!(
            code::instruction_to_string(&concatted),
            code::instruction_to_string(&actual)
        );

        for (i, instruction) in actual.iter().enumerate() {
            assert_eq!(instruction, &concatted[i])
        }
    }

    fn test_bool_object(expected: bool, actual: &object::ObjectType) {
        match actual {
            ObjectType::BoolObj(x) => assert_eq!(expected, *x),
            _ => panic!("expected only bool objects"),
        }
    }

    fn test_integer_object(expected: f64, actual: &object::ObjectType) {
        match actual {
            ObjectType::IntegerObj(x) => assert_eq!(expected, *x),
            _ => panic!("expected only integer objects"),
        }
    }

    fn test_string_object(expected: &str, actual: &object::ObjectType) {
        match actual {
            ObjectType::StringObj(s) => assert_eq!(expected, *s),
            _ => panic!("expected only string objects"),
        }
    }

    fn test_constants(expected: Vec<Box<dyn Any>>, actual: &mut Vec<ObjectType>) {
        assert_eq!(expected.len(), actual.len());

        for (i, constant) in expected.into_iter().enumerate() {
            if constant.is::<f64>() {
                test_integer_object(*constant.downcast::<f64>().unwrap(), &actual[i]);
                continue;
            }
            if constant.is::<bool>() {
                test_bool_object(*constant.downcast::<bool>().unwrap(), &actual[i]);
                continue;
            }
            if constant.is::<&str>() {
                test_string_object(*constant.downcast::<&str>().unwrap(), &actual[i]);
                continue;
            }
            if constant.is::<Vec<Instructions>>() {
                let expected = *constant.downcast::<Vec<Instructions>>().unwrap();
                println!("exp --- {expected:?}");
                if let ObjectType::CompileFunction(ins, _, _) = &actual[i] {
                    test_instructions(expected, ins);
                    continue;
                } else {
                    panic!("expected instructions!");
                }
            }
            todo!("not yet set up for testing");
        }
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let program = test_setup!(&test.input);

            let mut constants = Vec::new();
            let symbol_table = SymbolTable::new();

            let mut compiler = Compiler::new(&mut constants, symbol_table);
            compiler.compile(program).unwrap();

            test_instructions(test.expected_instructions, &compiler.current_instructions());
            test_constants(test.expected_constants, compiler.constants);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "1 + 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1; 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Pop),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 - 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Sub),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 * 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Mul),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "2 / 1",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Div),
                    make::it!(&Op::Pop),
                ],
                (2.0, 1.0)
            ),
            compiler_test_case!(
                "-1",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Minus),
                    make::it!(&Op::Pop),
                ],
                (1.0)
            ),
        ]);
    }

    #[test]
    fn test_boolean_expressions() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "true",
                vec![make::it!(&Op::True), make::it!(&code::Op::Pop)]
            ),
            compiler_test_case!(
                "false",
                vec![make::it!(&Op::False), make::it!(&code::Op::Pop)]
            ),
            compiler_test_case!(
                "1 > 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::GreaterThan),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 < 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::GreaterThan),
                    make::it!(&Op::Pop),
                ],
                (2.0, 1.0)
            ),
            compiler_test_case!(
                "1 == 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Equal),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 != 2",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::NotEqual),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "true != false",
                vec![
                    make::it!(&Op::True),
                    make::it!(&Op::False),
                    make::it!(&Op::NotEqual),
                    make::it!(&Op::Pop),
                ]
            ),
            compiler_test_case!(
                "!true",
                vec![
                    make::it!(&Op::True),
                    make::it!(&Op::Bang),
                    make::it!(&Op::Pop),
                ]
            ),
        ]);
    }

    #[test]
    fn test_conditionals() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "if (true) { 10 }; 3333;",
                vec![
                    make::it!(&Op::True),
                    make::it!(&Op::JumpNotTruthy, vec![10]),
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Jump, vec![11]),
                    make::it!(&Op::Null),
                    make::it!(&Op::Pop),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Pop),
                ],
                (10.0, 3333.0)
            ),
            compiler_test_case!(
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    make::it!(&Op::True),
                    make::it!(&Op::JumpNotTruthy, vec![10]),
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Jump, vec![13]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Pop),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Pop),
                ],
                (10.0, 20.0, 3333.0)
            ),
        ]);
    }

    #[test]
    fn test_global_let_statements() {
        run_compiler_tests(vec![
            compiler_test_case!(
                r#"
                    let one = 1;
                    let two = 2;
                "#,
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::SetGlobal, vec![1]),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                r#"
                    let one = 1;
                    one;
                "#,
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::GetGlobal, vec![0]),
                    make::it!(&Op::Pop),
                ],
                (1.0)
            ),
            compiler_test_case!(
                r#"
                    let one = 1;
                    let two = one;
                    two;
                "#,
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::GetGlobal, vec![0]),
                    make::it!(&Op::SetGlobal, vec![1]),
                    make::it!(&Op::GetGlobal, vec![1]),
                    make::it!(&Op::Pop),
                ],
                (1.0)
            ),
        ]);
    }

    #[test]
    fn test_string_expressions() {
        run_compiler_tests(vec![
            compiler_test_case!(
                r#""monkey""#,
                vec![make::it!(&Op::Constant, vec![0]), make::it!(&Op::Pop),],
                ("monkey")
            ),
            compiler_test_case!(
                r#""mon" + "key""#,
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Pop),
                ],
                ("mon", "key")
            ),
        ]);
    }

    #[test]
    fn test_array_literals() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "[]",
                vec![make::it!(&Op::Array, vec![0]), make::it!(&Op::Pop)]
            ),
            compiler_test_case!(
                "[1, 2, 3]",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Array, vec![3]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0)
            ),
            compiler_test_case!(
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Constant, vec![3]),
                    make::it!(&Op::Sub),
                    make::it!(&Op::Constant, vec![4]),
                    make::it!(&Op::Constant, vec![5]),
                    make::it!(&Op::Mul),
                    make::it!(&Op::Array, vec![3]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
            ),
        ]);
    }
    //
    #[test]
    fn test_hash_literals() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "{}",
                vec![make::it!(&Op::Hash, vec![0]), make::it!(&Op::Pop)]
            ),
            compiler_test_case!(
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Constant, vec![3]),
                    make::it!(&Op::Constant, vec![4]),
                    make::it!(&Op::Constant, vec![5]),
                    make::it!(&Op::Hash, vec![6]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
            ),
            compiler_test_case!(
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Constant, vec![3]),
                    make::it!(&Op::Constant, vec![4]),
                    make::it!(&Op::Constant, vec![5]),
                    make::it!(&Op::Mul),
                    make::it!(&Op::Hash, vec![4]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
            ),
        ]);
    }

    #[test]
    fn test_index_expressions() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "[1,2,3][1 + 1]",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Array, vec![3]),
                    make::it!(&Op::Constant, vec![3]),
                    make::it!(&Op::Constant, vec![4]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Index),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0, 1.0, 1.0)
            ),
            compiler_test_case!(
                "{1: 2}[2 - 1]",
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Hash, vec![2]),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Constant, vec![3]),
                    make::it!(&Op::Sub),
                    make::it!(&Op::Index),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 2.0, 1.0)
            ),
        ]);
    }

    #[test]
    fn test_functions() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "fn() { return 5 + 10 }",
                vec![make::it!(&Op::Closure, vec![2, 0]), make::it!(&Op::Pop)],
                (
                    5.0,
                    10.0,
                    vec![
                        make::it!(&Op::Constant, vec![0]),
                        make::it!(&Op::Constant, vec![1]),
                        make::it!(&Op::Add),
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
            compiler_test_case!(
                "fn() { 5 + 10 }",
                vec![make::it!(&Op::Closure, vec![2, 0]), make::it!(&Op::Pop)],
                (
                    5.0,
                    10.0,
                    vec![
                        make::it!(&Op::Constant, vec![0]),
                        make::it!(&Op::Constant, vec![1]),
                        make::it!(&Op::Add),
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
            compiler_test_case!(
                "fn() { 1; 2 }",
                vec![make::it!(&Op::Closure, vec![2, 0]), make::it!(&Op::Pop)],
                (
                    1.0,
                    2.0,
                    vec![
                        make::it!(&Op::Constant, vec![0]),
                        make::it!(&Op::Pop),
                        make::it!(&Op::Constant, vec![1]),
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
            compiler_test_case!(
                "fn() { 24 }()",
                vec![
                    make::it!(&Op::Closure, vec![1, 0]),
                    make::it!(&Op::Call, vec![0]),
                    make::it!(&Op::Pop)
                ],
                (
                    24.0,
                    vec![
                        make::it!(&Op::Constant, vec![0]),
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
            compiler_test_case!(
                r#"
                    let noArg = fn() { 24 };
                    noArg();
                "#,
                vec![
                    make::it!(&Op::Closure, vec![1, 0]), // the compiled function
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::GetGlobal, vec![0]),
                    make::it!(&Op::Call, vec![0]),
                    make::it!(&Op::Pop)
                ],
                (
                    24.0,
                    vec![
                        make::it!(&Op::Constant, vec![0]), // the literal 24
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
        ]);
    }

    #[test]
    fn test_functions_without_return_value() {
        run_compiler_tests(vec![compiler_test_case!(
            "fn() { }",
            vec![make::it!(&Op::Closure, vec![0, 0]), make::it!(&Op::Pop)],
            (vec![make::it!(&Op::Return)])
        )]);
    }

    #[test]
    fn test_compiler_scopes() {
        let mut constants = Vec::new();
        let symbol_table = SymbolTable::new();

        let mut compiler = Compiler::new(&mut constants, symbol_table);

        assert_eq!(compiler.scope_index, 0);
        compiler.emit(&Op::Mul, vec![]);

        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);
        compiler.emit(&Op::Sub, vec![]);

        assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 1);

        let last = compiler.scopes[compiler.scope_index]
            .last_instruction
            .clone();
        assert_eq!(last.opcode, Op::Sub);

        assert!(compiler.symbol_table.outer.as_ref().is_some());

        compiler.leave_scope();

        assert!(compiler.symbol_table.outer.as_ref().is_none());
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(&Op::Add, vec![]);
        assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 2);
        let last = compiler.scopes[compiler.scope_index]
            .last_instruction
            .clone();
        assert_eq!(last.opcode, Op::Add);

        let previous = compiler.scopes[compiler.scope_index]
            .previous_instruction
            .clone();
        assert_eq!(previous.opcode, Op::Mul);
    }

    #[test]
    fn test_let_statement_scopes() {
        run_compiler_tests(vec![
            compiler_test_case!(
                r#"
                    let num = 55;
                    fn() { num }
                "#,
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::Closure, vec![1, 0]),
                    make::it!(&Op::Pop),
                ],
                (
                    55.0,
                    vec![
                        make::it!(&Op::GetGlobal, vec![0]),
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
            compiler_test_case!(
                r#"
                    fn() {
                        let num = 55;
                        num
                    }
                "#,
                vec![make::it!(&Op::Closure, vec![1, 0]), make::it!(&Op::Pop),],
                (
                    55.0,
                    vec![
                        make::it!(&Op::Constant, vec![0]),
                        make::it!(&Op::SetLocal, vec![0]),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
            compiler_test_case!(
                r#"
                    fn() {
                        let a = 55;
                        let b = 77;
                        a + b
                    }
                "#,
                vec![make::it!(&Op::Closure, vec![2, 0]), make::it!(&Op::Pop)],
                (
                    55.0,
                    77.0,
                    vec![
                        make::it!(&Op::Constant, vec![0]),
                        make::it!(&Op::SetLocal, vec![0]),
                        make::it!(&Op::Constant, vec![1]),
                        make::it!(&Op::SetLocal, vec![1]),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::GetLocal, vec![1]),
                        make::it!(&Op::Add),
                        make::it!(&Op::ReturnValue)
                    ]
                )
            ),
        ]);
    }

    #[test]
    fn test_function_calls() {
        run_compiler_tests(vec![
            compiler_test_case!(
                r#"
                    let oneArg = fn(a) {
                        a
                    };
                    oneArg(24);
                "#,
                vec![
                    make::it!(&Op::Closure, vec![0, 0]),
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::GetGlobal, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Call, vec![1]),
                    make::it!(&Op::Pop),
                ],
                (
                    vec![
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::ReturnValue)
                    ],
                    24.0
                )
            ),
            compiler_test_case!(
                r#"
                    let manyArg = fn(a, b, c) { 
                        a; b; c;
                    };
                    manyArg(24, 25, 26);
                "#,
                vec![
                    make::it!(&Op::Closure, vec![0, 0]),
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::GetGlobal, vec![0]),
                    make::it!(&Op::Constant, vec![1]),
                    make::it!(&Op::Constant, vec![2]),
                    make::it!(&Op::Constant, vec![3]),
                    make::it!(&Op::Call, vec![3]),
                    make::it!(&Op::Pop),
                ],
                (
                    vec![
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Pop),
                        make::it!(&Op::GetLocal, vec![1]),
                        make::it!(&Op::Pop),
                        make::it!(&Op::GetLocal, vec![2]),
                        make::it!(&Op::ReturnValue)
                    ],
                    24.0,
                    25.0,
                    26.0
                )
            ),
        ]);
    }

    #[test]
    fn test_builtins() {
        run_compiler_tests(vec![
            compiler_test_case!(
                r#"
                    len([]);
                    push([], 1);
                "#,
                vec![
                    make::it!(&Op::GetBuiltin, vec![0]),
                    make::it!(&Op::Array, vec![0]),
                    make::it!(&Op::Call, vec![1]),
                    make::it!(&Op::Pop),
                    make::it!(&Op::GetBuiltin, vec![5]),
                    make::it!(&Op::Array, vec![0]),
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Call, vec![2]),
                    make::it!(&Op::Pop)
                ],
                (1.0)
            ),
            compiler_test_case!(
                "fn() { len([]) }",
                vec![make::it!(&Op::Closure, vec![0, 0]), make::it!(&Op::Pop)],
                (vec![
                    make::it!(&Op::GetBuiltin, vec![0]),
                    make::it!(&Op::Array, vec![0]),
                    make::it!(&Op::Call, vec![1]),
                    make::it!(&Op::ReturnValue)
                ])
            ),
        ]);
    }

    #[test]
    fn test_closures() {
        run_compiler_tests(vec![
            compiler_test_case!(
                r#"
                fn(a) {
                    fn(b) {
                        a + b
                    }
                }
            "#,
                vec![make::it!(&Op::Closure, vec![1, 0]), make::it!(&Op::Pop),],
                (
                    vec![
                        make::it!(&Op::GetFree, vec![0]),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Add),
                        make::it!(&Op::ReturnValue),
                    ],
                    vec![
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Closure, vec![0, 1]),
                        make::it!(&Op::ReturnValue),
                    ]
                )
            ),
            compiler_test_case!(
                r#"
                    fn(a) {
                        fn(b) {
                            fn(c) {
                                a + b + c
                            }
                        }
                    };
                "#,
                vec![make::it!(&Op::Closure, vec![2, 0]), make::it!(&Op::Pop),],
                (
                    vec![
                        make::it!(&Op::GetFree, vec![0]),
                        make::it!(&Op::GetFree, vec![1]),
                        make::it!(&Op::Add),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Add),
                        make::it!(&Op::ReturnValue),
                    ],
                    vec![
                        make::it!(&Op::GetFree, vec![0]),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Closure, vec![0, 2]),
                        make::it!(&Op::ReturnValue),
                    ],
                    vec![
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Closure, vec![1, 1]),
                        make::it!(&Op::ReturnValue),
                    ]
                )
            ),
            compiler_test_case!(
                r#"let global = 55;

                fn() {
                    let a = 66;

                    fn() {
                        let b = 77;

                        fn() {
                            let c = 88;

                            global + a + b + c;
                        }
                    }
                }"#,
                vec![
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::SetGlobal, vec![0]),
                    make::it!(&Op::Closure, vec![6, 0]),
                    make::it!(&Op::Pop),
                ],
                (
                    55.0,
                    66.0,
                    77.0,
                    88.0,
                    vec![
                        make::it!(&Op::Constant, vec![3]),
                        make::it!(&Op::SetLocal, vec![0]),
                        make::it!(&Op::GetGlobal, vec![0]),
                        make::it!(&Op::GetFree, vec![0]),
                        make::it!(&Op::Add),
                        make::it!(&Op::GetFree, vec![1]),
                        make::it!(&Op::Add),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Add),
                        make::it!(&Op::ReturnValue),
                    ],
                    vec![
                        make::it!(&Op::Constant, vec![2]),
                        make::it!(&Op::SetLocal, vec![0]),
                        make::it!(&Op::GetFree, vec![0]),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Closure, vec![4, 2]),
                        make::it!(&Op::ReturnValue),
                    ],
                    vec![
                        make::it!(&Op::Constant, vec![1]),
                        make::it!(&Op::SetLocal, vec![0]),
                        make::it!(&Op::GetLocal, vec![0]),
                        make::it!(&Op::Closure, vec![5, 1]),
                        make::it!(&Op::ReturnValue),
                    ]
                )
            ),
        ])
    }

    #[test]
    fn test_recursive_closures() {
        run_compiler_tests(vec![compiler_test_case!(
            r#"
                let countDown = fn(x) { countDown(x - 1); };
                countDown(1);
                "#,
            vec![
                make::it!(&Op::Closure, vec![1, 0]),
                make::it!(&Op::SetGlobal, vec![0]),
                make::it!(&Op::GetGlobal, vec![0]),
                make::it!(&Op::Constant, vec![2]),
                make::it!(&Op::Call, vec![1]),
                make::it!(&Op::Pop),
            ],
            (
                1.0,
                vec![
                    make::it!(&Op::CurrentClosure),
                    make::it!(&Op::GetLocal, vec![0]),
                    make::it!(&Op::Constant, vec![0]),
                    make::it!(&Op::Sub),
                    make::it!(&Op::Call, vec![1]),
                    make::it!(&Op::ReturnValue)
                ],
                1.0
            )
        )]);
    }
}
