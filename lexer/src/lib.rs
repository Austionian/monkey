use std::char;
use token::{Token, look_up_ident};

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
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
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

fn is_letter(ch: char) -> bool {
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        const EXPECTED: [Token; 9] = [
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for (i, tt) in EXPECTED.iter().enumerate() {
            let tok = lexer.next_token();

            if tok != *tt {
                panic!(
                    "{:?} - token_type wrong. expected = {:?}, got = {:?}",
                    EXPECTED[i], tt, tok
                )
            }
        }
    }

    #[test]
    fn test_next_token_complex() {
        let input = r#"let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            "foobar";
            "foo bar";
            [1, 2];
            {"foo": "bar"};
            true || false;
            "#;

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::Not_eq,
            Token::Int(9),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::Semicolon,
            Token::String("foo bar".to_string()),
            Token::Semicolon,
            Token::Lbracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::Rbracket,
            Token::Semicolon,
            Token::Lbrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::Rbrace,
            Token::Semicolon,
            Token::True,
            Token::Or,
            Token::False,
            Token::Semicolon,
            Token::Eof,
        ];
        let mut lexer = Lexer::new(input);

        for (i, tt) in expected.iter().enumerate() {
            let tok = lexer.next_token();

            if tok != *tt {
                panic!(
                    "{:?} - token_type wrong. expected = {:?}, got = {:?}",
                    expected[i], tt, tok
                )
            }
        }
    }
}
