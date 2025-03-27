pub mod symbol_table;
use crate::{
    ast::{self, BlockStatement, Expression, LetStatement, Statement},
    code::{self, make, Op},
    object::{self, ObjectType},
    token::Token,
};
use symbol_table::SymbolTable;
use thiserror::Error;

pub struct Compiler<'a, 'b> {
    pub instructions: code::Instructions,
    pub constants: &'a mut Vec<object::ObjectType>,
    // very last instruction
    pub last_instruction: EmittedInstruction,
    // instruction before last_instruction
    pub previous_instruction: EmittedInstruction,
    pub symbol_table: &'a mut SymbolTable<'b>,
}

pub struct Environment<'a> {
    pub constants: Vec<object::ObjectType>,
    pub symbol_table: SymbolTable<'a>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),
        }
    }
}

#[derive(Default, Clone)]
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
    fn compile(
        &self,
        compiler: &mut crate::compiler::Compiler,
    ) -> Result<(), crate::compiler::CompilerError> {
        match self {
            Self::IntExpression(t) => {
                let integer = t.to_owned().into();
                let i = compiler.add_constant(integer);
                let _ = compiler.emit(&code::Op::Constant, vec![i]);
            }
            Self::BoolExpression(t) => match t {
                Token::TRUE => {
                    compiler.emit(&Op::True, vec![]);
                }
                Token::FALSE => {
                    compiler.emit(&Op::False, vec![]);
                }
                _ => unreachable!("Only bools should be in bool expressions"),
            },
            Self::InfixExpression((operator, left, right)) => {
                if operator == &Token::LT {
                    // Flip the order rather than make an OP_LESS_THAN
                    right.as_ref().compile(compiler)?;
                    left.as_ref().compile(compiler)?;
                    compiler.emit(&Op::GreaterThan, vec![]);

                    return Ok(());
                }
                left.as_ref().compile(compiler)?;
                right.as_ref().compile(compiler)?;

                match operator {
                    Token::PLUS => compiler.emit(&Op::Add, vec![]),
                    Token::MINUS => compiler.emit(&Op::Sub, vec![]),
                    Token::SLASH => compiler.emit(&Op::Div, vec![]),
                    Token::ASTERISK => compiler.emit(&Op::Mul, vec![]),
                    Token::GT => compiler.emit(&Op::GreaterThan, vec![]),
                    Token::EQ => compiler.emit(&Op::Equal, vec![]),
                    Token::NOT_EQ => compiler.emit(&Op::NotEqual, vec![]),
                    _ => todo!(),
                };
            }
            Self::PrefixExpression((operator, right)) => {
                right.as_ref().compile(compiler)?;

                match operator {
                    Token::BANG => compiler.emit(&Op::Bang, vec![]),
                    Token::MINUS => compiler.emit(&Op::Minus, vec![]),
                    _ => todo!(),
                };
            }
            Self::IfExpression(condition, consequence, alternative) => {
                condition.as_ref().compile(compiler)?;

                // emit with bogus jump value
                let jump_not_truthy = compiler.emit(&Op::JumpNotTruthy, vec![9999]);

                consequence.as_ref().compile(compiler)?;

                if compiler.last_instruction_is_pop() {
                    compiler.remove_last_pop();
                }

                // emit a jump with a bogus value
                let jump_position = compiler.emit(&Op::Jump, vec![9999]);

                let after_consequence_position = compiler.instructions.len();
                compiler.change_operand(jump_not_truthy, after_consequence_position);

                if let Some(alternative) = alternative {
                    alternative.as_ref().compile(compiler)?;

                    if compiler.last_instruction_is_pop() {
                        compiler.remove_last_pop();
                    }
                } else {
                    compiler.emit(&Op::Null, vec![]);
                }

                let after_alternative_position = compiler.instructions.len();
                compiler.change_operand(jump_position, after_alternative_position);
            }
            Self::IdentExpression(ident) => {
                let name = if let Token::IDENT(name) = ident {
                    name
                } else {
                    return Err(CompilerError::InvalidToken(ident.clone()));
                };
                let symbol = compiler
                    .symbol_table
                    .resolve(name)
                    .ok_or(CompilerError::UndefinedVariable)?;
                compiler.emit(&Op::GetGlobal, vec![symbol.index]);
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
            _ => todo!(),
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
            Self::ReturnStatement(_return_statement) => todo!(),
        }
    }
}

impl Compile for LetStatement {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), CompilerError> {
        self.value.compile(compiler)?;

        let name = if let Token::IDENT(name) = &self.name {
            name
        } else {
            return Err(CompilerError::InvalidToken(self.name.clone()));
        };
        let symbol = compiler.symbol_table.define(name.to_string());
        compiler.emit(&Op::SetGlobal, vec![symbol.index]);
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

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn new(constants: &'a mut Vec<ObjectType>, symbol_table: &'a mut SymbolTable<'b>) -> Self
    where
        'b: 'a,
    {
        Self {
            instructions: Vec::new(),
            constants,
            last_instruction: EmittedInstruction::default(),
            previous_instruction: EmittedInstruction::default(),
            symbol_table,
        }
    }

    pub fn compile(&mut self, node: ast::Program) -> Result<(), CompilerError> {
        for statement in node.statements {
            statement.compile(self)?;
        }

        Ok(())
    }

    fn remove_last_pop(&mut self) {
        self.instructions = self.instructions[..self.last_instruction.position].to_vec();
        self.last_instruction = self.previous_instruction.clone();
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.opcode == Op::Pop
    }

    pub fn add_constant(&mut self, obj: object::ObjectType) -> usize {
        self.constants.push(obj);
        return self.constants.len() - 1;
    }

    pub fn emit(&mut self, op: &Op, operands: Vec<usize>) -> usize {
        // TODO: this still doesn't make sense, why a vec of ops
        // maybe this should be generic like make? And only the correct
        // sizes get passed in?
        let mut ins: Vec<u8> = vec![];
        for width in op.lookup_widths() {
            match width {
                2 => {
                    ins = make::it!(op, operands.iter().map(|x| *x as u16));
                }
                _ => todo!(),
            };
        }

        let pos;
        if operands.is_empty() {
            pos = self.add_instruction(&mut vec![*op as u8]);
        } else {
            pos = self.add_instruction(&mut ins);
        }

        self.set_last_instruction(op, pos);

        pos
    }

    fn change_operand(&mut self, op_position: usize, operand: usize) {
        let op = self.instructions[op_position];
        let new_instruction = make::it!(&op.into(), vec![operand as u16]);

        self.replace_instruction(op_position, new_instruction);
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: Vec<u8>) {
        for i in 0..new_instruction.len() {
            self.instructions[position + i] = new_instruction[i]
        }
    }

    fn set_last_instruction(&mut self, op: &Op, position: usize) {
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction {
            opcode: *op,
            position,
        };

        self.previous_instruction = previous;
        self.last_instruction = last;
    }

    fn add_instruction(&mut self, ins: &mut Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.append(ins);

        pos_new_instruction
    }

    pub fn bytecode(self) -> ByteCode {
        ByteCode {
            instructions: self.instructions,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser, test_setup};
    use core::panic;
    use object::ObjectType;
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

        ($input:expr, $instructions:expr, ($($constant:expr), +)) => {{
            let mut expected_constants = Vec::<Box<dyn Any>>::new();
            $(
                expected_constants.push(Box::new($constant));
            )+
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

    fn test_instructions(expected: Vec<code::Instructions>, actual: code::Instructions) {
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
                return test_integer_object(*constant.downcast::<f64>().unwrap(), &actual[i]);
            }
            if constant.is::<bool>() {
                return test_bool_object(*constant.downcast::<bool>().unwrap(), &actual[i]);
            }
            if constant.is::<&str>() {
                return test_string_object(*constant.downcast::<&str>().unwrap(), &actual[i]);
            }
        }
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let program = test_setup!(&test.input);

            let mut constants = Vec::new();
            let mut symbol_table = SymbolTable::new();

            let mut compiler = Compiler::new(&mut constants, &mut symbol_table);
            compiler.compile(program).unwrap();

            test_instructions(test.expected_instructions, compiler.instructions);
            test_constants(test.expected_constants, compiler.constants);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        run_compiler_tests(vec![
            compiler_test_case!(
                "1 + 2",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1; 2",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Pop),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 - 2",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Sub),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 * 2",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Mul),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "2 / 1",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Div),
                    make::it!(&Op::Pop),
                ],
                (2.0, 1.0)
            ),
            compiler_test_case!(
                "-1",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
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
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::GreaterThan),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 < 2",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::GreaterThan),
                    make::it!(&Op::Pop),
                ],
                (2.0, 1.0)
            ),
            compiler_test_case!(
                "1 == 2",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Equal),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                "1 != 2",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
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
                    make::it!(&Op::JumpNotTruthy, vec![10u16]),
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Jump, vec![11u16]),
                    make::it!(&Op::Null),
                    make::it!(&Op::Pop),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Pop),
                ],
                (10.0, 3333.0)
            ),
            compiler_test_case!(
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    make::it!(&Op::True),
                    make::it!(&Op::JumpNotTruthy, vec![10u16]),
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Jump, vec![13u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Pop),
                    make::it!(&Op::Constant, vec![2u16]),
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
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::SetGlobal, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::SetGlobal, vec![1u16]),
                ],
                (1.0, 2.0)
            ),
            compiler_test_case!(
                r#"
                    let one = 1;
                    one;
                    "#,
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::SetGlobal, vec![0u16]),
                    make::it!(&Op::GetGlobal, vec![0u16]),
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
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::SetGlobal, vec![0u16]),
                    make::it!(&Op::GetGlobal, vec![0u16]),
                    make::it!(&Op::SetGlobal, vec![1u16]),
                    make::it!(&Op::GetGlobal, vec![1u16]),
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
                vec![make::it!(&Op::Constant, vec![0u16]), make::it!(&Op::Pop),],
                ("monkey")
            ),
            compiler_test_case!(
                r#""mon" + "key""#,
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
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
                vec![make::it!(&Op::Array, vec![0u16]), make::it!(&Op::Pop)]
            ),
            compiler_test_case!(
                "[1, 2, 3]",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Constant, vec![2u16]),
                    make::it!(&Op::Array, vec![3u16]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0)
            ),
            compiler_test_case!(
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Constant, vec![2u16]),
                    make::it!(&Op::Constant, vec![3u16]),
                    make::it!(&Op::Sub),
                    make::it!(&Op::Constant, vec![4u16]),
                    make::it!(&Op::Constant, vec![5u16]),
                    make::it!(&Op::Mul),
                    make::it!(&Op::Array, vec![3u16]),
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
                vec![make::it!(&Op::Hash, vec![0u16]), make::it!(&Op::Pop)]
            ),
            compiler_test_case!(
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Constant, vec![2u16]),
                    make::it!(&Op::Constant, vec![3u16]),
                    make::it!(&Op::Constant, vec![4u16]),
                    make::it!(&Op::Constant, vec![5u16]),
                    make::it!(&Op::Hash, vec![6u16]),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
            ),
            compiler_test_case!(
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Constant, vec![2u16]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Constant, vec![3u16]),
                    make::it!(&Op::Constant, vec![4u16]),
                    make::it!(&Op::Constant, vec![5u16]),
                    make::it!(&Op::Mul),
                    make::it!(&Op::Hash, vec![4u16]),
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
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Constant, vec![2u16]),
                    make::it!(&Op::Array, vec![3u16]),
                    make::it!(&Op::Constant, vec![3u16]),
                    make::it!(&Op::Constant, vec![4u16]),
                    make::it!(&Op::Add),
                    make::it!(&Op::Index),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 3.0, 1.0, 1.0)
            ),
            compiler_test_case!(
                "{1: 2}[2 - 1]",
                vec![
                    make::it!(&Op::Constant, vec![0u16]),
                    make::it!(&Op::Constant, vec![1u16]),
                    make::it!(&Op::Hash, vec![2u16]),
                    make::it!(&Op::Constant, vec![2u16]),
                    make::it!(&Op::Constant, vec![3u16]),
                    make::it!(&Op::Sub),
                    make::it!(&Op::Index),
                    make::it!(&Op::Pop),
                ],
                (1.0, 2.0, 2.0, 1.0)
            ),
        ]);
    }
}
