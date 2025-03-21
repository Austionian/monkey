use std::{collections::HashMap, str::from_utf8_unchecked};

type SymbolScope<'a> = &'a str;

// If I don't do it this way I'd need to it to be &'static, and I think
// we'll be dynamically creating scopes
const SCOPE: [u8; 6] = *b"GLOBAL";
const GLOBAL_SCOPE: SymbolScope = unsafe { from_utf8_unchecked(&SCOPE) };

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
    name: String,
    scope: SymbolScope<'a>,
    pub index: usize,
}

impl<'a> PartialEq for Symbol<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.scope == other.scope && self.index == other.index
    }
}

#[derive(Debug)]
pub struct SymbolTable<'a> {
    store: HashMap<String, Symbol<'a>>,
    num_definitions: usize,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol<'a> {
        let symbol = Symbol {
            name: name.clone(),
            index: self.num_definitions,
            scope: GLOBAL_SCOPE,
        };
        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &'a str) -> Option<&Symbol<'a>> {
        self.store.get(name)
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

        let mut global = SymbolTable::new();
        assert_eq!(global.define("a".to_string()), *expected.get("a").unwrap());
        assert_eq!(global.define("b".to_string()), *expected.get("b").unwrap());
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
}
