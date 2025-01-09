use crate::{
    ast::{BlockStatement, TokenLiteral},
    token::Token,
};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

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
    FunctionObj(Function),
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
            Self::FunctionObj(f) => f.to_string(),
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
            Self::FunctionObj(_) => write!(f, "FUNCTION"),
        }
    }
}

#[derive(Clone, PartialEq, Default, Debug)]
pub struct Environment {
    pub store: HashMap<String, ObjectType>,
    pub inner_store: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            inner_store: None,
        }
    }

    pub fn get(&self, name: &str) -> Option<&ObjectType> {
        let value = self.store.get(name);
        if value.is_none() {
            if let Some(inner_store) = &self.inner_store {
                return inner_store.get(name);
            }
        }
        value
    }

    pub fn set(&mut self, name: &str, value: ObjectType) -> ObjectType {
        self.store.insert(name.to_string(), value.clone());
        value
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub parameters: Vec<Token>,
    pub body: BlockStatement,
    pub inner_env: Environment,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::new();
        buffer.push_str(&format!(
            "fn ({}) ",
            self.parameters
                .iter()
                .map(|p| p.token_literal())
                .collect::<Vec<_>>()
                .join(", ")
        ));
        buffer.push_str(self.body.to_string().as_str());
        write!(f, "{buffer}")
    }
}
