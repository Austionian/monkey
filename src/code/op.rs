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

    GetLocal,
    SetLocal,

    GetFree,

    GetBuiltin,

    Array,
    Hash,
    Index,

    Call,
    Return,
    ReturnValue,

    Closure,
    CurrentClosure,
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
        match self {
            Self::Pop => write!(f, "OpPop"),
            Self::Constant => write!(f, "OpConstant"),
            Self::Add => write!(f, "OpAdd"),
            Self::Sub => write!(f, "OpSub"),
            Self::Mul => write!(f, "OpMul"),
            Self::Div => write!(f, "OpDiv"),
            Self::True => write!(f, "OpTrue"),
            Self::False => write!(f, "OpFalse"),
            Self::Equal => write!(f, "OpEqual"),
            Self::NotEqual => write!(f, "OpNotEqual"),
            Self::GreaterThan => write!(f, "OpGreaterThan"),
            Self::Minus => write!(f, "OpMinus"),
            Self::Bang => write!(f, "OpBang"),
            Self::Jump => write!(f, "OpJump"),
            Self::JumpNotTruthy => write!(f, "OpJumpNotTruthy"),
            Self::Null => write!(f, "OpNull"),
            Self::GetGlobal => write!(f, "OpGetGlobal"),
            Self::SetGlobal => write!(f, "OpSetGlobal"),
            Self::Array => write!(f, "OpArray"),
            Self::Hash => write!(f, "OpHash"),
            Self::Index => write!(f, "OpIndex"),
            Self::Call => write!(f, "OpCall"),
            Self::ReturnValue => write!(f, "OpReturnValue"),
            Self::Return => write!(f, "OpReturn"),
            Self::GetLocal => write!(f, "OpGetLocal"),
            Self::SetLocal => write!(f, "OpSetLocal"),
            Self::GetBuiltin => write!(f, "OpGetBuiltin"),
            Self::Closure => write!(f, "OpClosure"),
            Self::GetFree => write!(f, "OpGetFree"),
            Self::CurrentClosure => write!(f, "OpCurrentClosure"),
        }
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
            | Self::Index
            | Self::ReturnValue
            | Self::Return
            | Self::CurrentClosure
            | Self::Null => vec![],

            Self::GetLocal | Self::SetLocal | Self::Call | Self::GetBuiltin | Self::GetFree => {
                vec![1]
            }

            Self::Constant
            | Self::Jump
            | Self::JumpNotTruthy
            | Self::GetGlobal
            | Self::SetGlobal
            | Self::Array
            | Self::Hash => vec![2],

            Self::Closure => vec![2, 1],
        }
    }
}
