use super::new_error;
use crate::object::{Object, ObjectType};
use std::{cell::LazyCell, collections::HashMap};

fn len(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 1 {
        return new_error(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        ObjectType::StringObj(string) => ObjectType::IntegerObj(string.len() as f64),
        _ => new_error(&format!(
            "argument to `len` not supported, got {}",
            args[0].r#type()
        )),
    }
}

pub const BUILTINS: LazyCell<HashMap<&'static str, fn(Vec<ObjectType>) -> ObjectType>> =
    LazyCell::new(|| {
        let mut builtins = HashMap::new();

        builtins.insert("len", len as fn(Vec<ObjectType>) -> ObjectType);

        builtins
    });
