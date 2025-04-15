use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

// If I don't do it this way I'd need to it to be &'static, and I think
// we'll be dynamically creating scopes
pub const GLOBAL_SCOPE: &str = "GLOBAL";
pub const LOCAL_SCOPE: &str = "LOCAL";
pub const BUILTIN_SCOPE: &str = "BUILTIN";
pub const FREE_SCOPE: &str = "FREE";
pub const FUNCTION_SCOPE: &str = "FUNCTION";

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
    store: Rc<RefCell<HashMap<String, Symbol>>>,
    pub num_definitions: usize,
    pub free_symbols: Rc<RefCell<Vec<Symbol>>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: Rc::new(RefCell::new(HashMap::new())),
            num_definitions: 0,
            free_symbols: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn new_enclosed(outer: Box<Self>) -> Self {
        Self {
            outer: Some(outer),
            store: Rc::new(RefCell::new(HashMap::new())),
            num_definitions: 0,
            free_symbols: Rc::new(RefCell::new(Vec::new())),
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

        self.store.borrow_mut().insert(name, symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn define_function_name(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            index: 0,
            scope: FUNCTION_SCOPE,
        };
        self.store.borrow_mut().insert(name, symbol.clone());

        symbol
    }

    // TODO: ugly!
    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        let rc = self.store.borrow_mut();
        let symbol = rc.get(name);
        if symbol.is_none() && self.outer.is_some() {
            if let Some(outer) = self.outer.as_ref() {
                let obj = outer.resolve(name);

                match obj {
                    None => return obj,
                    Some(obj) => {
                        if obj.scope == GLOBAL_SCOPE || obj.scope == BUILTIN_SCOPE {
                            return Some(obj);
                        }

                        let free = self.define_free(&obj, rc);
                        return Some(free);
                    }
                }
            }
        }
        if symbol.is_some() {
            let symbol = symbol.unwrap().clone();
            Some(symbol)
        } else {
            None
        }
    }

    pub fn define_builtin(&mut self, index: usize, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.into(),
            index,
            scope: BUILTIN_SCOPE,
        };
        self.store.borrow_mut().insert(name.into(), symbol.clone());
        symbol
    }

    // TODO: ugly!
    fn define_free(
        &self,
        original: &Symbol,
        mut store: RefMut<'_, HashMap<String, Symbol>>,
    ) -> Symbol {
        self.free_symbols.borrow_mut().push(original.clone());

        let symbol = Symbol {
            name: original.name.clone(),
            index: self.free_symbols.as_ref().borrow().len() - 1,
            scope: FREE_SCOPE,
        };

        let _ = store.insert(symbol.name.clone(), symbol.clone());

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
            assert_eq!(result, symbol);
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
            assert_eq!(*symbol, result);
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
            assert_eq!(*symbol, result);
        });

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
            if let Some(ref outer) = second_local.outer {
                let result = outer.resolve(&symbol.name).unwrap();
                assert_eq!(*symbol, result);
            } else {
                panic!("expected outer scope!")
            }
        })
    }

    #[test]
    fn test_define_resolve_builtins() {
        let mut global = SymbolTable::new();
        let expected = vec![
            Symbol {
                name: "a".into(),
                scope: BUILTIN_SCOPE,
                index: 0,
            },
            Symbol {
                name: "c".into(),
                scope: BUILTIN_SCOPE,
                index: 1,
            },
            Symbol {
                name: "e".into(),
                scope: BUILTIN_SCOPE,
                index: 2,
            },
            Symbol {
                name: "f".into(),
                scope: BUILTIN_SCOPE,
                index: 3,
            },
        ];

        for (i, v) in expected.iter().enumerate() {
            global.define_builtin(i, &v.name);
        }

        let first_local = SymbolTable::new_enclosed(Box::new(global));
        let second_local = SymbolTable::new_enclosed(Box::new(first_local));

        for symbol in &expected {
            let result = second_local.resolve(&symbol.name).unwrap();
            assert_eq!(result, *symbol)
        }

        for symbol in &expected {
            if let Some(ref outer) = second_local.outer {
                let result = outer.resolve(&symbol.name).unwrap();
                assert_eq!(result, *symbol);

                for symbol in &expected {
                    if let Some(ref outer) = outer.outer {
                        let result = outer.resolve(&symbol.name).unwrap();
                        assert_eq!(result, *symbol)
                    }
                }
            }
        }
    }

    #[test]
    fn test_resolve_free() {
        let mut global = SymbolTable::new();
        global.define("a".into());
        global.define("b".into());

        let mut first_local = SymbolTable::new_enclosed(Box::new(global));
        first_local.define("c".into());
        first_local.define("d".into());

        let mut second_local = SymbolTable::new_enclosed(Box::new(first_local));
        second_local.define("e".into());
        second_local.define("f".into());

        struct Test {
            expected_symbols: Vec<Symbol>,
            expected_free_symbols: Vec<Symbol>,
        }
        let tests = vec![Test {
            expected_symbols: vec![
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
            ],
            expected_free_symbols: vec![],
        }];

        for tt in tests {
            if let Some(ref outer) = second_local.outer {
                for sym in tt.expected_symbols {
                    let result = outer.resolve(&sym.name).unwrap();

                    assert_eq!(result, sym);
                }
                assert_eq!(
                    outer.free_symbols.borrow().len(),
                    tt.expected_free_symbols.len()
                );

                for (i, sym) in tt.expected_free_symbols.iter().enumerate() {
                    assert_eq!(outer.free_symbols.borrow()[i], *sym)
                }
            }
        }

        let tests = vec![Test {
            expected_symbols: vec![
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
                    scope: FREE_SCOPE,
                    index: 0,
                },
                Symbol {
                    name: "d".into(),
                    scope: FREE_SCOPE,
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
            ],
            expected_free_symbols: vec![
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
            ],
        }];

        for tt in tests {
            for sym in tt.expected_symbols {
                let result = second_local.resolve(&sym.name).unwrap();

                assert_eq!(result, sym);
            }

            assert_eq!(
                second_local.free_symbols.borrow().len(),
                tt.expected_free_symbols.len()
            );

            for (i, sym) in tt.expected_free_symbols.iter().enumerate() {
                assert_eq!(second_local.free_symbols.borrow()[i], *sym)
            }
        }
    }

    #[test]
    fn test_unresolvable_free() {
        let mut global = SymbolTable::new();
        global.define("a".into());

        let mut first_local = SymbolTable::new_enclosed(Box::new(global));
        first_local.define("c".into());

        let mut second_local = SymbolTable::new_enclosed(Box::new(first_local));
        second_local.define("e".into());
        second_local.define("f".into());

        let expected = vec![
            Symbol {
                name: "a".into(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "c".into(),
                scope: FREE_SCOPE,
                index: 0,
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

        for sym in expected {
            let result = second_local.resolve(&sym.name).unwrap();
            assert_eq!(result, sym);
        }

        let expected_unresolvable = vec!["b", "d"];

        for name in expected_unresolvable {
            assert!(second_local.resolve(&name).is_none());
        }
    }

    #[test]
    fn test_define_and_resolve_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a".into());

        let expected = Symbol {
            name: "a".into(),
            scope: FUNCTION_SCOPE,
            index: 0,
        };

        let result = global.resolve("a").unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_shadow_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a".into());
        global.define("a".into());

        let expected = Symbol {
            name: "a".into(),
            scope: GLOBAL_SCOPE,
            index: 0,
        };

        let result = global.resolve("a").unwrap();
        assert_eq!(result, expected);
    }
}
