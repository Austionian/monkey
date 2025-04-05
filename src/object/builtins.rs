use super::{Object, ObjectType};
use std::sync::LazyLock;

struct Builtin {
    name: String,
    builtin: fn(Vec<ObjectType>) -> ObjectType,
}

static BUILTINS: LazyLock<Vec<Builtin>> = LazyLock::new(|| {
    let mut builtins = Vec::new();

    builtins.push(Builtin {
        name: "len".into(),
        builtin: len,
    });

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

fn new_error(msg: &str) -> ObjectType {
    ObjectType::ErrorObj(msg.to_string())
}

pub fn get_builtin_by_name(name: &str) -> Option<&Builtin> {
    for builtin in BUILTINS.iter() {
        if builtin.name == name {
            return Some(builtin);
        }
    }

    None
}
