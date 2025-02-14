use std::{
    cell::LazyCell,
    clone::Clone,
    collections::HashMap,
    io::{Cursor, Seek, Write},
};

pub type Instructions = Vec<Opcode>;
pub type Opcode = u8;

const OP_CONSTANT: Opcode = 0;

#[derive(Clone)]
struct Definition {
    name: &'static str,
    operand_widths: Vec<u16>,
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

fn make(op: &Opcode, operands: impl IntoIterator<Item = u16>) -> Vec<u8> {
    if let Some(def) = DEFINITIONS.get(op) {
        let mut instruction_len = 1;

        for w in def.operand_widths.iter() {
            instruction_len += w;
        }

        let mut instruction = vec![0u8; instruction_len.into()];
        instruction[0] = *op;

        let mut offset = 1u16;
        for (i, o) in operands.into_iter().enumerate() {
            let width = def.operand_widths[i];
            match width {
                2 => {
                    let mut cursor = Cursor::new(&mut instruction);
                    cursor.seek_relative(offset.into()).unwrap();
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
}
