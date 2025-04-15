mod op;

pub use op::Op;
use std::{
    io::{Cursor, Seek, Write},
    mem::transmute,
};

pub type Instructions = Vec<Opcode>;
pub type Opcode = u8;

#[allow(dead_code)] // this isn't really dead
pub fn internal_make_no_operands(op: &Op) -> Vec<u8> {
    let widths = op.lookup_widths();
    let mut instruction_len = 1;

    for w in widths.iter() {
        instruction_len += w;
    }

    let mut instruction = vec![0u8; instruction_len.into()];
    instruction[0] = *op as u8;

    instruction
}

pub fn internal_make(op: &Op, operands: Vec<usize>) -> Vec<u8> {
    let widths = op.lookup_widths();
    let mut instruction_len = 1;

    for width in widths.iter() {
        instruction_len += width;
    }

    let mut instruction = vec![0u8; instruction_len.into()];
    instruction[0] = (*op).into();

    let mut offset = 1;

    for (i, operand) in operands.into_iter().enumerate() {
        let width = widths[i];
        let mut cursor = Cursor::new(&mut instruction);
        cursor.seek_relative(offset.into()).unwrap();
        match width {
            1 => cursor.write_all(&(operand as u8).to_be_bytes()).unwrap(),
            2 => cursor.write_all(&(operand as u16).to_be_bytes()).unwrap(),
            _ => unreachable!(),
        }

        offset += width;
    }

    instruction
}

pub mod make {
    /// Allows me to not have to operands when none are needed
    /// to make and removes the need to put the operands in an
    /// option.
    macro_rules! it {
        ($op:expr) => {
            $crate::code::internal_make_no_operands($op)
        };
        ($op:expr, $operands:expr) => {
            $crate::code::internal_make($op, $operands)
        };
    }

    pub(crate) use it;
}

pub fn read_u16(ins: &[u8]) -> u16 {
    let arr: [u8; 2] = ins[0..2].try_into().unwrap();
    u16::from_be_bytes(arr)
}

pub fn read_u8(ins: &[u8]) -> u8 {
    ins[0]
}

pub fn read_operands(op: &Op, ins: &[u8]) -> (Vec<u32>, usize) {
    let widths = op.lookup_widths();
    let mut operands = vec![];
    let mut offset = 0;

    for width in widths {
        match width {
            2 => operands.push(read_u16(&ins[offset..offset + 2]).into()),
            1 => operands.push(read_u8(&ins[offset..]).into()),
            _ => todo!(),
        };

        offset += width as usize;
    }

    (operands, offset)
}

pub fn instruction_to_string(ins: &Instructions) -> String {
    let mut out = String::new();

    let mut i = 0;
    while i < ins.len() {
        //let def = look_up(&ins[i]).unwrap();

        let op = unsafe { transmute(ins[i]) };
        let (operands, read) = read_operands(&op, &ins[i + 1..]);

        out.push_str(&format!(
            "{:04} {}\n",
            i,
            format_instruction(&op, operands).unwrap()
        ));

        i += 1 + read;
    }

    out
}

fn format_instruction(op: &Op, operands: Vec<u32>) -> Result<String, String> {
    let operand_count = op.lookup_widths().len();

    if operands.len() != operand_count {
        return Err("operand len doesn't match defined len.".to_string());
    }

    match operand_count {
        0 => Ok(format!("Op{:?}", op)),
        1 => Ok(format!("Op{:?} {}", op, operands[0])),
        2 => Ok(format!("Op{:?} {} {}", op, operands[0], operands[1])),
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct Test<T> {
        op: Op,
        operands: Vec<T>,
        expected: Vec<Opcode>,
    }

    #[test]
    fn test_make() {
        let tests = vec![
            Test {
                op: Op::Constant,
                operands: vec![65534],
                expected: vec![Op::Constant.into(), 255, 254],
            },
            Test {
                op: Op::Add,
                operands: vec![],
                expected: vec![Op::Add.into()],
            },
            Test {
                op: Op::Closure,
                operands: vec![65534, 255],
                expected: vec![Op::Closure.into(), 255, 254, 255],
            },
        ];

        for test in tests {
            let instruction = make::it!(&test.op, test.operands);

            assert_eq!(instruction.len(), test.expected.len());

            for (i, _) in test.expected.iter().enumerate() {
                assert_eq!(instruction[i], test.expected[i]);
            }
        }
    }

    #[test]
    fn test_make_u8() {
        let tests = vec![Test {
            op: Op::GetLocal,
            operands: vec![255],
            expected: vec![Op::GetLocal.into(), 255],
        }];

        for test in tests {
            let instruction = make::it!(&test.op, test.operands);

            assert_eq!(instruction.len(), test.expected.len());

            for (i, _) in test.expected.iter().enumerate() {
                assert_eq!(instruction[i], test.expected[i]);
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions: Vec<Instructions> = vec![
            make::it!(&Op::Add),
            make::it!(&Op::GetLocal, vec![1]),
            make::it!(&Op::Constant, vec![2]),
            make::it!(&Op::Constant, vec![65535]),
            make::it!(&Op::Closure, vec![65535, 255]),
        ];

        let expected = r#"0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
0009 OpClosure 65535 255
"#;

        let concated = instructions
            .iter()
            .flatten()
            .map(|v| v.to_owned())
            .collect::<Vec<_>>();

        assert_eq!(expected, instruction_to_string(&concated));
    }

    #[test]
    fn test_read_operands() {
        struct Test<T> {
            op: Op,
            operands: Vec<T>,
            bytes_read: usize,
        }

        let tests = vec![
            Test {
                op: Op::Constant,
                operands: vec![65535],
                bytes_read: 2,
            },
            Test {
                op: Op::Closure,
                operands: vec![65535, 255],
                bytes_read: 3,
            },
        ];

        for t in tests.iter() {
            let instruction = make::it!(&t.op, t.operands.clone());

            let (operands_read, n) = read_operands(&t.op, &instruction[1..]);
            assert_eq!(n, t.bytes_read);

            for (i, want) in t.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want as u32);
            }
        }

        let tests = vec![Test {
            op: Op::GetLocal,
            operands: vec![255],
            bytes_read: 1,
        }];

        for t in tests.iter() {
            let instruction = make::it!(&t.op, t.operands.clone());

            let (operands_read, n) = read_operands(&t.op, &instruction[1..]);
            assert_eq!(n, t.bytes_read);

            for (i, want) in t.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want as u32);
            }
        }
    }
}
