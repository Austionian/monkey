use std::{
    cell::LazyCell,
    clone::Clone,
    collections::HashMap,
    io::{Cursor, Seek, Write},
};

pub type Instructions = Vec<Opcode>;
pub type Opcode = u8;

pub const OP_CONSTANT: Opcode = 0;

#[derive(Clone)]
pub struct Definition {
    pub name: &'static str,
    pub operand_widths: Vec<u16>,
}

const DEFINITIONS: LazyCell<HashMap<Opcode, Definition>> = LazyCell::new(|| {
    let mut definitions = HashMap::new();

    definitions.insert(
        OP_CONSTANT,
        Definition {
            name: "OpConstant",
            operand_widths: vec![2],
        },
    );

    definitions
});

fn look_up(op: &Opcode) -> Option<Definition> {
    DEFINITIONS.get(op).cloned()
}

pub fn make(op: &Opcode, operands: impl IntoIterator<Item = u16>) -> Vec<u8> {
    if let Some(def) = DEFINITIONS.get(op) {
        let mut instruction_len = 1;

        for w in def.operand_widths.iter() {
            instruction_len += w;
        }

        let mut instruction = vec![0u8; instruction_len.into()];
        instruction[0] = *op;

        let mut offset = 1;
        for (i, o) in operands.into_iter().enumerate() {
            let width = def.operand_widths[i];
            match width {
                2 => {
                    let mut cursor = Cursor::new(&mut instruction);
                    cursor.seek_relative(offset as i64).unwrap();
                    cursor.write(&o.to_be_bytes()).unwrap();
                }
                _ => todo!(),
            }
            offset += width;
        }

        instruction
    } else {
        vec![]
    }
}

fn read_u16(ins: &[u8]) -> u16 {
    let arr: [u8; 2] = ins.try_into().unwrap();
    15u16.to_be();
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

fn instruction_to_string(ins: &Instructions) -> String {
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
        1 => Ok(format!("{} {}", def.name, operands[0])),
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use core::panic;

    struct Test {
        op: Opcode,
        operands: Vec<u16>,
        expected: Vec<Opcode>,
    }

    #[test]
    fn test_make() {
        let tests = vec![Test {
            op: OP_CONSTANT,
            operands: vec![65534],
            expected: vec![OP_CONSTANT, 255, 254],
        }];

        for test in tests {
            let instruction = make(&test.op, test.operands);

            assert_eq!(instruction.len(), test.expected.len());

            for (i, _) in test.expected.iter().enumerate() {
                assert_eq!(instruction[i], test.expected[i]);
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions: Vec<Instructions> = vec![
            make(&OP_CONSTANT, vec![1]),
            make(&OP_CONSTANT, vec![2]),
            make(&OP_CONSTANT, vec![65535]),
        ];

        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
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
            let instruction = make(&t.op, t.operands.clone());
            let def = look_up(&t.op).unwrap();

            let (operands_read, n) = read_operands(&def, &instruction[1..]);
            assert_eq!(n, t.bytes_read);

            for (i, want) in t.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want);
            }
        }
    }
}
