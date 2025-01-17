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
        ObjectType::ArrayObj(array) => ObjectType::IntegerObj(array.len() as f64),
        _ => new_error(&format!(
            "argument to `len` not supported, got {}",
            args[0].r#type()
        )),
    }
}

fn first(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 1 {
        return new_error(&format!(
            "wrong number of arguements. got={}, want=1",
            args.len()
        ));
    }

    if let ObjectType::ArrayObj(array) = &args[0] {
        array.first().unwrap_or(&ObjectType::NullObj).clone()
    } else {
        new_error(&format!(
            "arguement to `first` must be ARRAY, got {}",
            args[0].r#type()
        ))
    }
}

fn last(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 1 {
        return new_error(&format!(
            "wrong number of arguements. got={}, want=1",
            args.len()
        ));
    }

    if let ObjectType::ArrayObj(array) = &args[0] {
        array.last().unwrap_or(&ObjectType::NullObj).clone()
    } else {
        new_error(&format!(
            "arguement to `last` must be ARRAY, got {}",
            args[0].r#type()
        ))
    }
}

fn rest(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 1 {
        return new_error(&format!(
            "wrong number of arguements. got={}, want=1",
            args.len()
        ));
    }

    if let ObjectType::ArrayObj(array) = &args[0] {
        if array.len() > 0 {
            ObjectType::ArrayObj(Vec::from_iter(array[1..].iter().cloned()))
        } else {
            ObjectType::NullObj
        }
    } else {
        new_error(&format!(
            "arguement to `rest` must be ARRAY, got {}",
            args[0].r#type()
        ))
    }
}

fn puts(args: Vec<ObjectType>) -> ObjectType {
    for arg in args {
        println!("{}", arg.inspect());
    }
    ObjectType::NullObj
}

pub const BUILTINS: LazyCell<HashMap<&'static str, fn(Vec<ObjectType>) -> ObjectType>> =
    LazyCell::new(|| {
        let mut builtins = HashMap::new();

        builtins.insert("len", len as fn(Vec<ObjectType>) -> ObjectType);
        builtins.insert("first", first);
        builtins.insert("last", last);
        builtins.insert("rest", rest);
        builtins.insert("puts", puts);

        builtins
    });
