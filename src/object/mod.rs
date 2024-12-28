use std::fmt::Display;

pub trait Object {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub enum ObjectType {
    IntegerObj,
    BoolObj,
    NullObj,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::IntegerObj => write!(f, "INTEGER"),
            ObjectType::BoolObj => write!(f, "BOOLEAN"),
            ObjectType::NullObj => write!(f, "NULL"),
        }
    }
}

pub struct Integer {
    pub value: f64,
}

impl Object for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn r#type(&self) -> ObjectType {
        ObjectType::IntegerObj
    }
}

pub struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn r#type(&self) -> ObjectType {
        ObjectType::BoolObj
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Null {}

impl Object for Null {
    fn r#type(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}
