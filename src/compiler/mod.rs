mod symbol_table;
use crate::{
    ast,
    code::{self, make, Opcode, DEFINITIONS, OP_CONSTANT, OP_FALSE, OP_POP, OP_TRUE},
    object,
    token::Token,
};

pub struct Compiler {
    pub instructions: code::Instructions,
    pub constants: Vec<object::ObjectType>,
    // very last instruction
    pub last_instruction: EmittedInstruction,
    // instruction before last_instruction
    pub previous_instruction: EmittedInstruction,
}

#[derive(Default, Clone)]
pub struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

pub struct ByteCode {
    pub instructions: code::Instructions,
    pub constants: Vec<object::ObjectType>,
}

#[derive(Debug)]
pub struct CompilerError {}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            last_instruction: EmittedInstruction::default(),
            previous_instruction: EmittedInstruction::default(),
        }
    }

    pub fn compile(&mut self, node: ast::Program) -> Result<(), CompilerError> {
        for statement in node.statements {
            self.compile_statement(&statement)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> Result<(), CompilerError> {
        match statement {
            ast::Statement::ExpressStatement(exp) => {
                self.compile_expression(&exp)?;
                self.emit(&OP_POP, vec![]);
                Ok(())
            }
            ast::Statement::LetStatement(exp) => Ok(self.compile_let_statement(exp)?),
            ast::Statement::BlockStatement(exp) => Ok(Self::compile_block(exp)?),
            ast::Statement::ReturnStatement(exp) => Ok(Self::compile_return(exp)?),
        }
    }

    fn compile_return(expression: &ast::ReturnStatement) -> Result<(), CompilerError> {
        todo!()
    }

    fn compile_block(expression: &ast::BlockStatement) -> Result<(), CompilerError> {
        todo!()
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), CompilerError> {
        match expression {
            ast::Expression::IntExpression(t) => {
                let integer = t.to_owned().into();
                let i = self.add_constant(integer);
                let _ = self.emit(&OP_CONSTANT, vec![i]);
            }
            ast::Expression::BoolExpression(t) => match t {
                Token::TRUE => {
                    self.emit(&OP_TRUE, vec![]);
                }
                Token::FALSE => {
                    self.emit(&OP_FALSE, vec![]);
                }
                _ => unreachable!("Only bools should be in bool expressions"),
            },
            ast::Expression::InfixExpression((operator, left, right)) => {
                if operator == &Token::LT {
                    // Flip the order rather than make an OP_LESS_THAN
                    self.compile_expression(right.as_ref())?;
                    self.compile_expression(left.as_ref())?;
                    self.emit(&code::OP_GREATER_THAN, vec![]);

                    return Ok(());
                }
                self.compile_expression(left.as_ref())?;
                self.compile_expression(right.as_ref())?;

                match operator {
                    Token::PLUS => self.emit(&code::OP_ADD, vec![]),
                    Token::MINUS => self.emit(&code::OP_SUB, vec![]),
                    Token::SLASH => self.emit(&code::OP_DIV, vec![]),
                    Token::ASTERISK => self.emit(&code::OP_MUL, vec![]),
                    Token::GT => self.emit(&code::OP_GREATER_THAN, vec![]),
                    Token::EQ => self.emit(&code::OP_EQUAL, vec![]),
                    Token::NOT_EQ => self.emit(&code::OP_NOT_EQUAL, vec![]),
                    _ => todo!(),
                };
            }
            ast::Expression::PrefixExpression((operator, right)) => {
                self.compile_expression(right.as_ref())?;

                match operator {
                    Token::BANG => self.emit(&code::OP_BANG, vec![]),
                    Token::MINUS => self.emit(&code::OP_MINUS, vec![]),
                    _ => todo!(),
                };
            }
            ast::Expression::IfExpression(condition, consequence, alternative) => {
                self.compile_expression(condition.as_ref())?;

                // emit with bogus jump value
                let jump_not_truthy = self.emit(&code::OP_JUMP_NOT_TRUTHY, vec![9999]);

                self.compile_block_statement(consequence.as_ref())?;

                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                // emit a jump with a bogus value
                let jump_position = self.emit(&code::OP_JUMP, vec![9999]);

                let after_consequence_position = self.instructions.len();
                self.change_operand(jump_not_truthy, after_consequence_position);

                if let Some(alternative) = alternative {
                    self.compile_block_statement(alternative)?;

                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }
                } else {
                    self.emit(&code::OP_NULL, vec![]);
                }

                let after_alternative_position = self.instructions.len();
                self.change_operand(jump_position, after_alternative_position);
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn compile_block_statement(
        &mut self,
        block: &ast::BlockStatement,
    ) -> Result<(), CompilerError> {
        for statement in block.statements.iter() {
            self.compile_statement(statement)?;
        }

        Ok(())
    }

    fn remove_last_pop(&mut self) {
        self.instructions = self.instructions[..self.last_instruction.position].to_vec();
        self.last_instruction = self.previous_instruction.clone();
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.opcode == code::OP_POP
    }

    fn add_constant(&mut self, obj: object::ObjectType) -> usize {
        self.constants.push(obj);
        return self.constants.len() - 1;
    }

    fn emit(&mut self, op: &Opcode, operands: Vec<usize>) -> usize {
        // TODO: this still doesn't make sense, why a vec of ops
        // maybe this should be generic like make? And only the correct
        // sizes get passed in?
        let mut ins: Vec<u8> = vec![];
        if let Some(def) = DEFINITIONS.get(op) {
            for width in def.operand_widths.iter() {
                match width {
                    2 => {
                        ins = make::it!(op, operands.iter().map(|x| *x as u16));
                    }
                    _ => todo!(),
                };
            }
        } else {
            panic!("Opcode not found!");
        }

        let pos;
        if operands.is_empty() {
            pos = self.add_instruction(&mut vec![*op]);
        } else {
            pos = self.add_instruction(&mut ins);
        }

        self.set_last_instruction(op, pos);

        pos
    }

    fn change_operand(&mut self, op_position: usize, operand: usize) {
        let op = self.instructions[op_position];
        let new_instruction = make::it!(&op, vec![operand as u16]);

        self.replace_instruction(op_position, new_instruction);
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: Vec<u8>) {
        for i in 0..new_instruction.len() {
            self.instructions[position + i] = new_instruction[i]
        }
    }

    fn set_last_instruction(&mut self, opcode: &Opcode, position: usize) {
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction {
            opcode: *opcode,
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

    fn compile_let_statement(
        &mut self,
        statement: &ast::LetStatement,
    ) -> Result<(), CompilerError> {
        self.compile_expression(&statement.value)
    }

    pub fn bytecode(self) -> ByteCode {
        ByteCode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{code::instruction_to_string, lexer::Lexer, parser::Parser, test_setup};
    use core::panic;
    use object::ObjectType;

    #[derive(Debug)]
    enum CompilerInterface {
        Int(f64),
        Bool(bool),
    }

    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<CompilerInterface>,
        expected_instructions: Vec<code::Instructions>,
    }

    /// helper function
    fn concat_instructions(expected: Vec<code::Instructions>) -> code::Instructions {
        expected.into_iter().flatten().collect()
    }

    fn test_instructions(expected: Vec<code::Instructions>, actual: code::Instructions) {
        let concatted = concat_instructions(expected);

        assert_eq!(
            instruction_to_string(&concatted),
            instruction_to_string(&actual)
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

    fn test_constants(expected: Vec<CompilerInterface>, actual: Vec<ObjectType>) {
        assert_eq!(expected.len(), actual.len());

        for (i, constant) in expected.iter().enumerate() {
            match constant {
                CompilerInterface::Int(x) => test_integer_object(*x, &actual[i]),
                CompilerInterface::Bool(b) => test_bool_object(*b, &actual[i]),
            }
        }
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let program = test_setup!(&test.input);
            let mut compiler = Compiler::new();
            compiler.compile(program).unwrap();

            test_instructions(test.expected_instructions, compiler.instructions);
            test_constants(test.expected_constants, compiler.constants);
        }
    }

    macro_rules! compiler_test_case {
        ($input:expr, $instructions:expr, ($($constant:expr), +)) => {{
            let mut expected_constants = Vec::new();
            $(
                expected_constants.push(CompilerInterface::Int($constant));
            )+
            CompilerTestCase {
                input: $input,
                expected_constants,
                expected_instructions: $instructions,
            }
        }};
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            compiler_test_case!(
                "1 + 2",
                vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_ADD),
                    make::it!(&code::OP_POP),
                ],
                (1.0, 2.0)
            ),
            CompilerTestCase {
                input: "1; 2",
                expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_POP),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "1 - 2",
                expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_SUB),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "1 * 2",
                expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_MUL),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "2 / 1",
                expected_constants: vec![CompilerInterface::Int(2.0), CompilerInterface::Int(1.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_DIV),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "-1",
                expected_constants: vec![CompilerInterface::Int(1.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_MINUS),
                    make::it!(&code::OP_POP),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "true",
                expected_constants: vec![],
                expected_instructions: vec![make::it!(&code::OP_TRUE), make::it!(&code::OP_POP)],
            },
            CompilerTestCase {
                input: "false",
                expected_constants: vec![],
                expected_instructions: vec![make::it!(&code::OP_FALSE), make::it!(&code::OP_POP)],
            },
            CompilerTestCase {
                input: "1 > 2",
                expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_GREATER_THAN),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "1 < 2",
                expected_constants: vec![CompilerInterface::Int(2.0), CompilerInterface::Int(1.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_GREATER_THAN),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "1 == 2",
                expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_EQUAL),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "1 != 2",
                expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_NOT_EQUAL),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "true != false",
                expected_constants: vec![],
                expected_instructions: vec![
                    make::it!(&code::OP_TRUE),
                    make::it!(&code::OP_FALSE),
                    make::it!(&code::OP_NOT_EQUAL),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "!true",
                expected_constants: vec![],
                expected_instructions: vec![
                    make::it!(&code::OP_TRUE),
                    make::it!(&code::OP_BANG),
                    make::it!(&code::OP_POP),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase {
                input: "if (true) { 10 }; 3333;",
                expected_constants: vec![
                    CompilerInterface::Int(10.0),
                    CompilerInterface::Int(3333.0),
                ],
                expected_instructions: vec![
                    make::it!(&code::OP_TRUE),
                    make::it!(&code::OP_JUMP_NOT_TRUTHY, vec![10u16]),
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_JUMP, vec![11u16]),
                    make::it!(&code::OP_NULL),
                    make::it!(&code::OP_POP),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: "if (true) { 10 } else { 20 }; 3333;",
                expected_constants: vec![
                    CompilerInterface::Int(10.0),
                    CompilerInterface::Int(20.0),
                    CompilerInterface::Int(3333.0),
                ],
                expected_instructions: vec![
                    make::it!(&code::OP_TRUE),
                    make::it!(&code::OP_JUMP_NOT_TRUTHY, vec![10u16]),
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_JUMP, vec![13u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_POP),
                    make::it!(&code::OP_CONSTANT, vec![2u16]),
                    make::it!(&code::OP_POP),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            CompilerTestCase {
                input: r#"
                let one = 1;
                let two = 2;
                "#,
                expected_constants: vec![CompilerInterface::Int(1.0), CompilerInterface::Int(2.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_SET_GLOBAL, vec![0u16]),
                    make::it!(&code::OP_CONSTANT, vec![1u16]),
                    make::it!(&code::OP_SET_GLOBAL, vec![1u16]),
                ],
            },
            CompilerTestCase {
                input: r#"
                let one = 1;
                one;
                "#,
                expected_constants: vec![CompilerInterface::Int(1.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_SET_GLOBAL, vec![0u16]),
                    make::it!(&code::OP_GET_GLOBAL, vec![0u16]),
                    make::it!(&code::OP_POP),
                ],
            },
            CompilerTestCase {
                input: r#"
                let one = 1;
                let two = one;
                two;
                "#,
                expected_constants: vec![CompilerInterface::Int(1.0)],
                expected_instructions: vec![
                    make::it!(&code::OP_CONSTANT, vec![0u16]),
                    make::it!(&code::OP_SET_GLOBAL, vec![0u16]),
                    make::it!(&code::OP_GET_GLOBAL, vec![0u16]),
                    make::it!(&code::OP_SET_GLOBAL, vec![1u16]),
                    make::it!(&code::OP_GET_GLOBAL, vec![1u16]),
                    make::it!(&code::OP_POP),
                ],
            },
        ];

        run_compiler_tests(tests);
    }
}
