use std::str::from_utf8_unchecked;

type SymbolScope<'a> = &'a str;

// If I don't do it this way I'd need to it to be &'static
const SCOPE: [u8; 6] = *b"GLOBAL";
const GLOBAL_SCOPE: SymbolScope = unsafe { from_utf8_unchecked(&SCOPE) };

struct Symbol<'a> {
    name: String,
    scope: SymbolScope<'a>,
    index: usize,
}
