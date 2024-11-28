use crate::{lexer, token::Token};
use std::{
    io::{self, Write},
    process,
};

const PROMPT: &'static str = ">> ";

pub fn start() {
    print!("{PROMPT}");
    let _ = io::stdout().flush();

    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();

    if buffer.trim() == "/bye" || buffer.trim() == "exit" {
        process::exit(0)
    }

    let mut lexer = lexer::Lexer::new(&buffer);

    let mut tok = Token::default();
    while tok != Token::EOF {
        tok = lexer.next_token();
        println!("out: {:?}", tok);
    }
}
