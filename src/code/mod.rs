use std::{
    cell::LazyCell,
    clone::Clone,
    collections::HashMap,
    fmt::Display,
    io::{Cursor, Seek, Write},
};

pub type Instructions = Vec<Opcode>;
pub type Opcode = u8;

pub const NONE: Option<Vec<u8>> = None;
// TODO: Should these just be an enum?
pub const OP_CONSTANT: Opcode = 0;
pub const OP_ADD: Opcode = 1;
pub const OP_POP: Opcode = 2;
pub const OP_SUB: Opcode = 3;
pub const OP_MUL: Opcode = 4;
pub const OP_DIV: Opcode = 5;
pub const OP_TRUE: Opcode = 6;
pub const OP_FALSE: Opcode = 7;
pub const OP_EQUAL: Opcode = 8;
pub const OP_NOT_EQUAL: Opcode = 9;
pub const OP_GREATER_THAN: Opcode = 10;
pub const OP_MINUS: Opcode = 11;
pub const OP_BANG: Opcode = 12;
pub const OP_JUMP: Opcode = 13;
pub const OP_JUMP_NOT_TRUTHY: Opcode = 14;
pub const OP_NULL: Opcode = 15;
pub const OP_GET_GLOBAL: Opcode = 16;
pub const OP_SET_GLOBAL: Opcode = 17;

#[derive(Clone, Debug)]
pub struct Definition {
    pub name: String,
    pub operand_widths: Vec<u8>,
}

pub const DEFINITIONS: LazyCell<HashMap<Opcode, Definition>> = LazyCell::new(|| {
    let mut definitions = HashMap::new();

    macro_rules! op_definition {
        ($val:tt, $widths:expr) => { op_definition!(@ $val, $val, $widths); };
        (@ $op_ident:ident, $op:expr, $widths:expr) => {{
            let name = stringify!($op_ident)
                .to_lowercase()
                .to_string()
                .split("_")
                .map(|v| {
                    let mut c = v.chars();
                    match c.next() {
                        None => String::new(),
                        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
                    }
                })
                .collect::<String>();

            definitions.insert(
                $op,
                Definition {
                    name,
                    operand_widths: vec![$widths],
                },
            );
        }};
        ($val:tt) => { op_definition!(@@ $val, $val); };
        (@@ $op_ident:ident, $op:expr) => {{
            let name = stringify!($op_ident)
                .to_lowercase()
                .to_string()
                .split("_")
                .map(|v| {
                    let mut c = v.chars();
                    match c.next() {
                        None => String::new(),
                        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
                    }
                })
                .collect::<String>();

            definitions.insert(
                $op,
                Definition {
                    name,
                    operand_widths: vec![],
                },
            );
        }};
    }

    op_definition!(OP_CONSTANT, 2);
    op_definition!(OP_ADD);
    op_definition!(OP_POP);
    op_definition!(OP_SUB);
    op_definition!(OP_MUL);
    op_definition!(OP_DIV);
    op_definition!(OP_TRUE);
    op_definition!(OP_FALSE);
    op_definition!(OP_EQUAL);
    op_definition!(OP_NOT_EQUAL);
    op_definition!(OP_GREATER_THAN);
    op_definition!(OP_MINUS);
    op_definition!(OP_BANG);
    op_definition!(OP_JUMP, 2);
    op_definition!(OP_JUMP_NOT_TRUTHY, 2);
    op_definition!(OP_NULL);
    op_definition!(OP_GET_GLOBAL, 2);
    op_definition!(OP_SET_GLOBAL, 2);

    definitions
});

fn look_up(op: &Opcode) -> Option<Definition> {
    DEFINITIONS.get(op).cloned()
}

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

pub fn make<const N: usize, T: Fixed<Bytes = [u8; N]>>(
    op: &Opcode,
    operands: Option<impl IntoIterator<Item = T>>,
) -> Vec<u8>
where
    T: Display,
{
    if let Some(def) = DEFINITIONS.get(op) {
        let mut instruction_len = 1;

        for w in def.operand_widths.iter() {
            instruction_len += w;
        }

        let mut instruction = vec![0u8; instruction_len.into()];
        instruction[0] = *op;

        let mut offset = 1;
        if let Some(operands) = operands {
            for (i, o) in operands.into_iter().enumerate() {
                let width = def.operand_widths[i];
                let mut cursor = Cursor::new(&mut instruction);
                cursor.seek_relative(offset as i64).unwrap();
                cursor.write_all(&o.to_be_bytes()).unwrap();

                offset += width;
            }
        }

        instruction
    } else {
        vec![]
    }
}

pub fn read_u16(ins: &[u8]) -> u16 {
    let arr: [u8; 2] = ins[0..2].try_into().unwrap();
    u16::from_be_bytes(arr)
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<u16>, usize) {
    let mut operands = vec![0u16; def.operand_widths.len()];
    let mut offset = 0;

    for (i, width) in def.operand_widths.iter().enumerate() {
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
        let def = look_up(&ins[i]).unwrap();

        let (operands, read) = read_operands(&def, &ins[i + 1..]);

        out.push_str(&format!(
            "{:04} {}\n",
            i,
            format_instruction(&def, operands).unwrap()
        ));

        i += 1 + read;
    }

    out
}

fn format_instruction(def: &Definition, operands: Vec<u16>) -> Result<String, String> {
    let operand_count = def.operand_widths.len();

    if operands.len() != operand_count {
        return Err("operand len doesn't match defined len.".to_string());
    }

    match operand_count {
        0 => Ok(format!("{}", def.name)),
        1 => Ok(format!("{} {}", def.name, operands[0])),
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct Test {
        op: Opcode,
        operands: Vec<u16>,
        expected: Vec<Opcode>,
    }

    #[test]
    fn test_make() {
        let tests = vec![
            Test {
                op: OP_CONSTANT,
                operands: vec![65534],
                expected: vec![OP_CONSTANT, 255, 254],
            },
            Test {
                op: OP_ADD,
                operands: vec![],
                expected: vec![OP_ADD],
            },
        ];

        for test in tests {
            let instruction = make(&test.op, Some(test.operands));

            assert_eq!(instruction.len(), test.expected.len());

            for (i, _) in test.expected.iter().enumerate() {
                assert_eq!(instruction[i], test.expected[i]);
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions: Vec<Instructions> = vec![
            make(&OP_ADD, NONE),
            make(&OP_CONSTANT, Some(vec![2u16])),
            make(&OP_CONSTANT, Some(vec![65535u16])),
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
            op: Opcode,
            operands: Vec<u16>,
            bytes_read: usize,
        }

        let tests = vec![Test {
            op: OP_CONSTANT,
            operands: vec![65535],
            bytes_read: 2,
        }];

        for t in tests.iter() {
            let instruction = make(&t.op, Some(t.operands.clone()));
            let def = look_up(&t.op).unwrap();

            let (operands_read, n) = read_operands(&def, &instruction[1..]);
            assert_eq!(n, t.bytes_read);

            for (i, want) in t.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want);
            }
        }
    }
}
