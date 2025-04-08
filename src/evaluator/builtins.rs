use crate::object::{self, ObjectType};
use std::{cell::LazyCell, collections::HashMap};

pub const BUILTINS: LazyCell<HashMap<&'static str, fn(Vec<ObjectType>) -> ObjectType>> =
    LazyCell::new(|| {
        let mut builtins = HashMap::new();

        builtins.insert("len", object::get_builtin_by_name("len").unwrap());
        builtins.insert("puts", object::get_builtin_by_name("puts").unwrap());
        builtins.insert("first", object::get_builtin_by_name("first").unwrap());
        builtins.insert("last", object::get_builtin_by_name("last").unwrap());
        builtins.insert("rest", object::get_builtin_by_name("rest").unwrap());
        builtins.insert("push", object::get_builtin_by_name("push").unwrap());

        builtins
    });
