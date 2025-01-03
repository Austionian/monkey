use std::fmt::{Debug, Display};

pub trait Object {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Clone, PartialEq, Default)]
pub enum ObjectType {
    IntegerObj(Integer),
    BoolObj(Boolean),
    #[default]
    NullObj,
    ReturnValueObj(ReturnValue),
}

impl Object for ObjectType {
    fn r#type(&self) -> ObjectType {
        self.clone()
    }

    fn inspect(&self) -> String {
        match self {
            Self::BoolObj(b) => b.inspect(),
            Self::IntegerObj(i) => i.inspect(),
            Self::NullObj => "NULL".to_string(),
            Self::ReturnValueObj(r) => r.inspect(),
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
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Integer {
    pub value: f64,
}

impl Object for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn r#type(&self) -> ObjectType {
        ObjectType::IntegerObj(Integer::default())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn r#type(&self) -> ObjectType {
        ObjectType::BoolObj(Boolean::default())
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Null {}

impl Object for Null {
    fn r#type(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Clone, PartialEq)]
pub struct ReturnValue {
    pub value: Box<ObjectType>,
}

impl Debug for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Return Value: {}", self.value)
    }
}

impl Default for ReturnValue {
    fn default() -> Self {
        ReturnValue {
            value: Box::new(ObjectType::default()),
        }
    }
}

impl Object for ReturnValue {
    fn r#type(&self) -> ObjectType {
        ObjectType::ReturnValueObj(ReturnValue::default())
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}
