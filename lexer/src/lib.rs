mod helpers;

use helpers::{is_digit, is_letter};
use std::char;
use token::{Token, look_up_ident};

/// The Monkey lexer.
///
/// Turns text into tokens.
///
/// ```
/// use lexer::Lexer;
/// use token::Token;
///
/// let input = "let a = 5;";
///
/// let expected: [Token; 6] = [
///     Token::Let,
///     Token::Ident("a".to_string()),
///     Token::Assign,
///     Token::Int(5),
///     Token::Semicolon,
///     Token::Eof,
/// ];
///
/// let mut lexer = Lexer::new(input);
///
/// for (i, tt) in expected.iter().enumerate() {
///     let tok = lexer.next_token();
///
///     if tok != *tt {
///         panic!(
///             "{:?} - token_type wrong. expected = {:?}, got = {:?}",
///             expected[i], tt, tok
///         )
///     }
/// }
/// ```
#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    // points to the char in the input that corresponds to the ch
    position: usize,
    // points to the next char in the input
    read_position: usize,
    // ascii only
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: u8::default(),
        };

        lexer.init();
        lexer
    }

    fn init(&mut self) {
        self.read_char();
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_white_space();
        self.skip_comments();

        let tok = match self.ch as char {
            '=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '|' => {
                if self.peek_char() == b'|' {
                    self.read_char();
                    Token::Or
                } else {
                    Token::Illegal(self.input[self.position..self.position + 1].to_string())
                }
            }
            '&' => {
                if self.peek_char() == b'&' {
                    self.read_char();
                    Token::And
                } else {
                    Token::Illegal(self.input[self.position..self.position + 1].to_string())
                }
            }
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            ',' => Token::Comma,
            '+' => {
                if self.peek_char() == b'+' {
                    self.read_char();
                    Token::PlusPlus
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.peek_char() == b'-' {
                    self.read_char();
                    Token::MinusMinus
                } else {
                    Token::Minus
                }
            }
            '!' => {
                if self.peek_char() == b'=' {
                    self.read_char();

                    Token::Not_eq
                } else {
                    Token::Bang
                }
            }
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '>' => Token::Gt,
            '<' => Token::Lt,
            '\0' => Token::Eof,
            ':' => Token::Colon,
            '"' => Token::String(self.read_string()),
            ch => {
                if is_letter(ch) {
                    let literal = self.read_identifier();
                    match look_up_ident(literal) {
                        Token::Ident(_) => return Token::Ident(literal.to_string()),
                        token => return token,
                    };
                } else if is_digit(ch) {
                    return Token::Int(self.read_number());
                }
                Token::Illegal(self.input[self.position..self.position + 1].to_string())
            }
        };

        self.read_char();
        tok
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> usize {
        let position = self.position;
        while is_digit(self.ch as char) {
            self.read_char();
        }

        self.input[position..self.position]
            .parse::<usize>()
            .unwrap()
    }

    fn skip_white_space(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        if self.ch as char == '/' && self.peek_char() == b'/' {
            while !(self.ch == b'\n' || self.ch == b'\r') {
                self.read_char();
            }
            // read the \n or \r
            self.read_char();
            self.skip_comments();
        } else {
            return;
        }
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.ch as char) {
            self.read_char();
        }

        &self.input[position..self.position]
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            *self.input.as_bytes().get(self.read_position).unwrap()
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = b'\0'
        } else {
            self.ch = *self.input.as_bytes().get(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
}
