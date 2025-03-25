use std::{clone::Clone, fmt::Display, mem::transmute};

#[derive(Default, Debug, Hash, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Op {
    #[default]
    Pop,
    Constant,

    Add,
    Sub,
    Mul,
    Div,

    True,
    False,

    Equal,
    NotEqual,
    GreaterThan,

    Minus,
    Bang,

    Jump,
    JumpNotTruthy,

    Null,

    GetGlobal,
    SetGlobal,

    Array,
    Hash,
}

impl Into<u8> for Op {
    fn into(self) -> u8 {
        self as u8
    }
}

impl Into<Op> for u8 {
    fn into(self) -> Op {
        // SAFETY: an Op should only ever be a u8. This could definitely blow up though.
        unsafe { transmute(self) }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Op")
    }
}

impl Op {
    pub fn lookup_widths(&self) -> Vec<u8> {
        match self {
            Self::Pop
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::True
            | Self::False
            | Self::Equal
            | Self::NotEqual
            | Self::GreaterThan
            | Self::Minus
            | Self::Bang
            | Self::Null => vec![],

            Self::Constant
            | Self::Jump
            | Self::JumpNotTruthy
            | Self::GetGlobal
            | Self::SetGlobal
            | Self::Array
            | Self::Hash => vec![2],
        }
    }
}
