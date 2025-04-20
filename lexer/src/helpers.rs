pub fn is_letter(ch: char) -> bool {
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
}

pub fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}
