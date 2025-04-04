use std::collections::HashMap;

// If I don't do it this way I'd need to it to be &'static, and I think
// we'll be dynamically creating scopes
pub const GLOBAL_SCOPE: &'static str = "GLOBAL";
const LOCAL_SCOPE: &'static str = "LOCAL";

#[derive(Debug, Clone)]
pub struct Symbol {
    name: String,
    pub scope: &'static str,
    pub index: usize,
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.scope == other.scope && self.index == other.index
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<Self>>,
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_enclosed(outer: Box<Self>) -> Self {
        Self {
            outer: Some(outer),
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol {
        let symbol = if self.outer.is_none() {
            Symbol {
                name: name.clone(),
                index: self.num_definitions,
                scope: GLOBAL_SCOPE,
            }
        } else {
            Symbol {
                name: name.clone(),
                index: self.num_definitions,
                scope: LOCAL_SCOPE,
            }
        };

        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        let symbol = self.store.get(name);
        if symbol.is_none() && self.outer.is_some() {
            if let Some(outer) = self.outer.as_ref() {
                return outer.resolve(name);
            }
        }
        symbol
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_define() {
        let mut expected = HashMap::new();
        expected.insert(
            "a".to_string(),
            Symbol {
                name: "a".to_string(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
        );
        expected.insert(
            "b".to_string(),
            Symbol {
                name: "b".to_string(),
                scope: GLOBAL_SCOPE,
                index: 1,
            },
        );
        expected.insert(
            "c".to_string(),
            Symbol {
                name: "c".to_string(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
        );
        expected.insert(
            "d".to_string(),
            Symbol {
                name: "d".to_string(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        );
        expected.insert(
            "e".to_string(),
            Symbol {
                name: "e".to_string(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
        );
        expected.insert(
            "f".to_string(),
            Symbol {
                name: "f".to_string(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        );

        let mut global = SymbolTable::new();
        assert_eq!(global.define("a".to_string()), *expected.get("a").unwrap());
        assert_eq!(global.define("b".to_string()), *expected.get("b").unwrap());

        let mut first_local = SymbolTable::new_enclosed(Box::new(global));
        assert_eq!(
            first_local.define("c".to_string()),
            *expected.get("c").unwrap()
        );
        assert_eq!(
            first_local.define("d".to_string()),
            *expected.get("d").unwrap()
        );

        let mut second_local = SymbolTable::new_enclosed(Box::new(first_local));
        assert_eq!(
            second_local.define("e".to_string()),
            *expected.get("e").unwrap()
        );
        assert_eq!(
            second_local.define("f".to_string()),
            *expected.get("f").unwrap()
        );
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let expected = vec![
            Symbol {
                name: "a".to_string(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "b".to_string(),
                scope: GLOBAL_SCOPE,
                index: 1,
            },
        ];

        for symbol in expected {
            let result = global.resolve(&symbol.name).unwrap();
            assert_eq!(*result, symbol);
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let mut local = SymbolTable::new_enclosed(Box::new(global));
        local.define("c".into());
        local.define("d".into());

        let expected = vec![
            Symbol {
                name: "a".into(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "b".into(),
                scope: GLOBAL_SCOPE,
                index: 1,
            },
            Symbol {
                name: "c".into(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "d".into(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        ];

        expected.iter().for_each(|symbol| {
            let result = local.resolve(&symbol.name).unwrap();
            assert_eq!(symbol, result);
        })
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let mut local = SymbolTable::new_enclosed(Box::new(global));
        local.define("c".into());
        local.define("d".into());

        let mut second_local = SymbolTable::new_enclosed(Box::new(local));
        second_local.define("e".into());
        second_local.define("f".into());

        let expected = vec![
            Symbol {
                name: "a".into(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "b".into(),
                scope: GLOBAL_SCOPE,
                index: 1,
            },
            Symbol {
                name: "c".into(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "d".into(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
            Symbol {
                name: "e".into(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "f".into(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        ];

        expected.iter().for_each(|symbol| {
            let result = second_local.resolve(&symbol.name).unwrap();
            assert_eq!(symbol, result);
        })
    }
}
