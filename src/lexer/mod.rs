use crate::token::{look_up_ident, Token};
use std::char;

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
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            '[' => Token::LBRACKET,
            ']' => Token::RBRACKET,
            ',' => Token::COMMA,
            '+' => Token::PLUS,
            '-' => Token::MINUS,
            '!' => {
                if self.peek_char() == b'=' {
                    self.read_char();

                    Token::NOT_EQ
                } else {
                    Token::BANG
                }
            }
            '*' => Token::ASTERISK,
            '/' => Token::SLASH,
            '>' => Token::GT,
            '<' => Token::LT,
            '\0' => Token::EOF,
            ':' => Token::COLON,
            '"' => Token::STRING(self.read_string()),
            ch => {
                if is_letter(ch) {
                    let literal = self.read_identifier();
                    match look_up_ident(literal) {
                        Token::IDENT(_) => return Token::IDENT(literal.to_string()),
                        token => return token,
                    };
                } else if is_digit(ch) {
                    return Token::INT(self.read_number());
                }
                Token::ILLEGAL(self.input[self.position..self.position + 1].to_string())
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
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::EOF,
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
            "#;

        let expected = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOT_EQ,
            Token::INT(9),
            Token::SEMICOLON,
            Token::STRING("foobar".to_string()),
            Token::SEMICOLON,
            Token::STRING("foo bar".to_string()),
            Token::SEMICOLON,
            Token::LBRACKET,
            Token::INT(1),
            Token::COMMA,
            Token::INT(2),
            Token::RBRACKET,
            Token::SEMICOLON,
            Token::LBRACE,
            Token::STRING("foo".to_string()),
            Token::COLON,
            Token::STRING("bar".to_string()),
            Token::RBRACE,
            Token::SEMICOLON,
            Token::EOF,
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
