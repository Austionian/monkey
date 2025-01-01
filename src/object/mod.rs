use std::fmt::Display;

pub trait Object {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Clone, PartialEq)]
pub enum ObjectType {
    IntegerObj(Integer),
    BoolObj(Boolean),
    NullObj(Null),
}

impl Object for ObjectType {
    fn r#type(&self) -> ObjectType {
        self.clone()
    }

    fn inspect(&self) -> String {
        match self {
            Self::BoolObj(b) => b.inspect(),
            Self::IntegerObj(i) => i.inspect(),
            Self::NullObj(n) => n.inspect(),
        }
    }
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::IntegerObj(_) => write!(f, "INTEGER"),
            ObjectType::BoolObj(_) => write!(f, "BOOLEAN"),
            ObjectType::NullObj(_) => write!(f, "NULL"),
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
        ObjectType::NullObj(Null::default())
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}
