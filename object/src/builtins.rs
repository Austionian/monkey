use super::{BuiltinFn, Object, ObjectType};
use std::sync::LazyLock;

pub struct Builtin {
    pub name: String,
    pub builtin: BuiltinFn,
}

pub static BUILTINS: LazyLock<Vec<Builtin>> = LazyLock::new(|| {
    let mut builtins = Vec::new();

    macro_rules! builtin {
        ($name:tt) => { builtin!(@ $name, $name); };
        (@ $name:ident, $fn_pointer:expr) => {{
            builtins.push(Builtin {
                name: stringify!($name).into(),
                builtin: $fn_pointer,
            });
        }};
    }

    builtin!(len);
    builtin!(puts);
    builtin!(first);
    builtin!(last);
    builtin!(rest);
    builtin!(push);

    builtins
});

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

fn puts(args: Vec<ObjectType>) -> ObjectType {
    for arg in args {
        println!("{}", arg.inspect());
    }
    ObjectType::NullObj
}

fn first(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 1 {
        return new_error(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    if let ObjectType::ArrayObj(array) = &args[0] {
        array.first().unwrap_or(&ObjectType::NullObj).clone()
    } else {
        new_error(&format!(
            "argument to `first` must be ARRAY, got {}",
            args[0].r#type()
        ))
    }
}

fn last(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 1 {
        return new_error(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    if let ObjectType::ArrayObj(array) = &args[0] {
        array.last().unwrap_or(&ObjectType::NullObj).clone()
    } else {
        new_error(&format!(
            "argument to `last` must be ARRAY, got {}",
            args[0].r#type()
        ))
    }
}

fn rest(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 1 {
        return new_error(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    if let ObjectType::ArrayObj(array) = &args[0] {
        if !array.is_empty() {
            ObjectType::ArrayObj(Vec::from_iter(array[1..].iter().cloned()))
        } else {
            ObjectType::NullObj
        }
    } else {
        new_error(&format!(
            "argument to `rest` must be ARRAY, got {}",
            args[0].r#type()
        ))
    }
}

fn push(args: Vec<ObjectType>) -> ObjectType {
    if args.len() != 2 {
        return new_error(&format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ));
    }
    if let ObjectType::ArrayObj(arr) = args[0].r#type() {
        let mut new_arr = vec![ObjectType::NullObj; arr.len()];
        new_arr.clone_from_slice(&arr);
        new_arr.push(args[1].clone());

        ObjectType::ArrayObj(new_arr)
    } else {
        new_error(&format!(
            "argument to `push` must be ARRAY, got {}",
            args[0].r#type()
        ))
    }
}

fn new_error(msg: &str) -> ObjectType {
    ObjectType::ErrorObj(msg.to_string())
}

pub fn get_builtin_by_name(name: &str) -> Option<fn(Vec<ObjectType>) -> ObjectType> {
    for builtin in BUILTINS.iter() {
        if builtin.name == name {
            return Some(builtin.builtin);
        }
    }

    None
}
