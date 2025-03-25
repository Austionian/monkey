mod op;

pub use op::Op;
use std::{
    io::{Cursor, Seek, Write},
    mem::transmute,
};

pub type Instructions = Vec<Opcode>;
pub type Opcode = u8;

pub trait Fixed {
    type Bytes;

    fn to_be_bytes(&self) -> Self::Bytes;
}

impl Fixed for u8 {
    type Bytes = [u8; 1];

    fn to_be_bytes(&self) -> Self::Bytes {
        u8::to_be_bytes(*self)
    }
}

impl Fixed for u16 {
    type Bytes = [u8; 2];

    fn to_be_bytes(&self) -> Self::Bytes {
        u16::to_be_bytes(*self)
    }
}

impl Fixed for u32 {
    type Bytes = [u8; 4];

    fn to_be_bytes(&self) -> Self::Bytes {
        u32::to_be_bytes(*self)
    }
}

impl Fixed for u64 {
    type Bytes = [u8; 8];

    fn to_be_bytes(&self) -> Self::Bytes {
        u64::to_be_bytes(*self)
    }
}

impl Fixed for usize {
    type Bytes = [u8; 8];

    fn to_be_bytes(&self) -> Self::Bytes {
        usize::to_be_bytes(*self)
    }
}

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

pub fn internal_make<const N: usize, T: Fixed<Bytes = [u8; N]>>(
    op: &Op,
    operands: impl IntoIterator<Item = T>,
) -> Vec<u8> {
    let def = op.lookup_widths();
    let mut instruction_len = 1;

    for w in def.iter() {
        instruction_len += w;
    }

    let mut instruction = vec![0u8; instruction_len.into()];
    instruction[0] = *op as u8;

    let mut offset = 1;

    for (i, o) in operands.into_iter().enumerate() {
        let width = def[i];
        let mut cursor = Cursor::new(&mut instruction);
        cursor.seek_relative(offset as i64).unwrap();
        cursor.write_all(&o.to_be_bytes()).unwrap();

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

pub fn read_operands(op: &Op, ins: &[u8]) -> (Vec<u16>, usize) {
    let widths = op.lookup_widths();
    let mut operands = vec![0u16; widths.len()];
    let mut offset = 0;

    for (i, width) in widths.iter().enumerate() {
        match width {
            2 => operands[i] = read_u16(&ins[offset..offset + *width as usize]),
            _ => todo!(),
        };

        offset += *width as usize;
    }

    (operands, offset.into())
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

fn format_instruction(op: &Op, operands: Vec<u16>) -> Result<String, String> {
    let operand_count = op.lookup_widths().len();

    if operands.len() != operand_count {
        return Err("operand len doesn't match defined len.".to_string());
    }

    match operand_count {
        0 => Ok(format!("Op{:?}", op)),
        1 => Ok(format!("Op{:?} {}", op, operands[0])),
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct Test {
        op: Op,
        operands: Vec<u16>,
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
    fn test_instructions_string() {
        let instructions: Vec<Instructions> = vec![
            make::it!(&Op::Add),
            make::it!(&Op::Constant, vec![2u16]),
            make::it!(&Op::Constant, vec![65535u16]),
        ];

        let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
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
        struct Test {
            op: Op,
            operands: Vec<u16>,
            bytes_read: usize,
        }

        let tests = vec![Test {
            op: Op::Constant,
            operands: vec![65535],
            bytes_read: 2,
        }];

        for t in tests.iter() {
            let instruction = make::it!(&t.op, t.operands.clone());

            let (operands_read, n) = read_operands(&t.op, &instruction[1..]);
            assert_eq!(n, t.bytes_read);

            for (i, want) in t.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want);
            }
        }
    }
}
