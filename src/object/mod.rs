use std::fmt::{Debug, Display};

pub trait Object {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Clone, PartialEq, Default, Debug)]
pub enum ObjectType {
    IntegerObj(f64),
    BoolObj(bool),
    #[default]
    NullObj,
    ReturnValueObj(Box<ObjectType>),
    ErrorObj(String),
}

impl Object for ObjectType {
    fn r#type(&self) -> ObjectType {
        self.clone()
    }

    fn inspect(&self) -> String {
        match self {
            Self::BoolObj(b) => b.to_string(),
            Self::IntegerObj(i) => i.to_string(),
            Self::NullObj => "NULL".to_string(),
            Self::ReturnValueObj(r) => r.inspect(),
            Self::ErrorObj(e) => e.to_string(),
        }
    }
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntegerObj(_) => write!(f, "INTEGER"),
            Self::BoolObj(_) => write!(f, "BOOLEAN"),
            Self::NullObj => write!(f, "NULL"),
            Self::ReturnValueObj(_) => write!(f, "RETURN"),
            Self::ErrorObj(_) => write!(f, "ERROR"),
        }
    }
}
