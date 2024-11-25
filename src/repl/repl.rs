use crate::{
    lexer,
    token::{Token, TokenType},
};
use std::io::{self, Write};

const PROMPT: &'static str = ">> ";

pub fn start() {
    print!("{PROMPT}");
    let _ = io::stdout().flush();

    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();

    let mut lexer = lexer::Lexer::new(&buffer);

    let mut tok = Token::default();
    while tok.r#type != TokenType::EOF {
        tok = lexer.next_token();
        println!("out: {:?}", tok);
    }
}
