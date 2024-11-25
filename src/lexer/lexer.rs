use crate::token::{look_up_ident, Token, TokenType};
use std::char;

#[derive(Debug)]
pub struct Lexer {
    input: &'static str,
    // points to the char in the input that corresponds to the ch
    position: usize,
    // points to the next char in the input
    read_position: usize,
    // ascii only
    ch: u8,
}

impl Lexer {
    fn new(input: &'static str) -> Self {
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

    fn next_token(&mut self) -> Token {
        self.skip_white_space();

        let tok = match self.ch as char {
            '=' => Token {
                literal: "=",
                r#type: TokenType::ASSIGN,
            },
            ';' => Token {
                literal: ";",
                r#type: TokenType::SEMICOLON,
            },
            '(' => Token {
                literal: "(",
                r#type: TokenType::LPAREN,
            },
            ')' => Token {
                literal: ")",
                r#type: TokenType::RPAREN,
            },
            '{' => Token {
                literal: "{",
                r#type: TokenType::LBRACE,
            },
            '}' => Token {
                literal: "}",
                r#type: TokenType::RBRACE,
            },
            ',' => Token {
                literal: ",",
                r#type: TokenType::COMMA,
            },
            '+' => Token {
                literal: "+",
                r#type: TokenType::PLUS,
            },
            '\0' => Token {
                literal: "\0",
                r#type: TokenType::EOF,
            },
            ch => {
                if is_letter(ch) {
                    let literal = self.read_identifier();
                    // return early since we've already read ahead in read_indentifier
                    return Token {
                        literal,
                        r#type: look_up_ident(literal),
                    };
                } else if is_digit(ch) {
                    return Token {
                        r#type: TokenType::INT,
                        literal: self.read_number(),
                    };
                }
                Token {
                    // TODO: clean up this leak. Program probably ends at this point so maybe
                    // leaking memory isn't the worst thing to do here.
                    literal: Box::leak(Box::new(String::from_utf8(vec![ch as u8]).unwrap())),
                    r#type: TokenType::ILLEGAL,
                }
            }
        };

        self.read_char();
        tok
    }

    fn read_number(&mut self) -> &'static str {
        let position = self.position;
        while is_digit(self.ch as char) {
            self.read_char();
        }

        &self.input[position..self.position]
    }

    fn skip_white_space(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> &'static str {
        let position = self.position;
        while is_letter(self.ch as char) {
            self.read_char();
        }

        &self.input[position..self.position]
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
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[derive(Debug)]
    struct TestTokens {
        expected_type: TokenType,
        expected_literal: &'static str,
    }

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        const EXPECTED: [TestTokens; 9] = [
            TestTokens {
                expected_type: TokenType::ASSIGN,
                expected_literal: "=",
            },
            TestTokens {
                expected_literal: "+",
                expected_type: TokenType::PLUS,
            },
            TestTokens {
                expected_type: TokenType::LPAREN,
                expected_literal: "(",
            },
            TestTokens {
                expected_literal: ")",
                expected_type: TokenType::RPAREN,
            },
            TestTokens {
                expected_type: TokenType::LBRACE,
                expected_literal: "{",
            },
            TestTokens {
                expected_type: TokenType::RBRACE,
                expected_literal: "}",
            },
            TestTokens {
                expected_type: TokenType::COMMA,
                expected_literal: ",",
            },
            TestTokens {
                expected_type: TokenType::SEMICOLON,
                expected_literal: ";",
            },
            TestTokens {
                expected_literal: "\0",
                expected_type: TokenType::EOF,
            },
        ];

        let mut lexer = Lexer::new(input);

        for (i, tt) in EXPECTED.iter().enumerate() {
            let tok = lexer.next_token();

            if tok.r#type != tt.expected_type {
                panic!(
                    "{:?} - token_type wrong. expected = {:?}, got = {:?}",
                    EXPECTED[i], tt.expected_type, tok.r#type
                )
            }
            if tok.literal != tt.expected_literal {
                panic!(
                    "{:?} - token_type wrong. expected = {:?}, got = {:?}",
                    EXPECTED[i], tt.expected_literal, tok.literal
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

            let result = add(five, ten);"#;

        const EXPECTED: [TestTokens; 36] = [
            TestTokens {
                expected_type: TokenType::LET,
                expected_literal: "let",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "five",
            },
            TestTokens {
                expected_type: TokenType::ASSIGN,
                expected_literal: "=",
            },
            TestTokens {
                expected_type: TokenType::INT,
                expected_literal: "5",
            },
            TestTokens {
                expected_type: TokenType::SEMICOLON,
                expected_literal: ";",
            },
            TestTokens {
                expected_type: TokenType::LET,
                expected_literal: "let",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "ten",
            },
            TestTokens {
                expected_type: TokenType::ASSIGN,
                expected_literal: "=",
            },
            TestTokens {
                expected_type: TokenType::INT,
                expected_literal: "10",
            },
            TestTokens {
                expected_type: TokenType::SEMICOLON,
                expected_literal: ";",
            },
            TestTokens {
                expected_type: TokenType::LET,
                expected_literal: "let",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "add",
            },
            TestTokens {
                expected_type: TokenType::ASSIGN,
                expected_literal: "=",
            },
            TestTokens {
                expected_type: TokenType::FUNCTION,
                expected_literal: "fn",
            },
            TestTokens {
                expected_type: TokenType::LPAREN,
                expected_literal: "(",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "x",
            },
            TestTokens {
                expected_type: TokenType::COMMA,
                expected_literal: ",",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "y",
            },
            TestTokens {
                expected_type: TokenType::RPAREN,
                expected_literal: ")",
            },
            TestTokens {
                expected_type: TokenType::LBRACE,
                expected_literal: "{",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "x",
            },
            TestTokens {
                expected_type: TokenType::PLUS,
                expected_literal: "+",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "y",
            },
            TestTokens {
                expected_type: TokenType::SEMICOLON,
                expected_literal: ";",
            },
            TestTokens {
                expected_type: TokenType::RBRACE,
                expected_literal: "}",
            },
            TestTokens {
                expected_type: TokenType::SEMICOLON,
                expected_literal: ";",
            },
            TestTokens {
                expected_type: TokenType::LET,
                expected_literal: "let",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "result",
            },
            TestTokens {
                expected_type: TokenType::ASSIGN,
                expected_literal: "=",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "add",
            },
            TestTokens {
                expected_type: TokenType::LPAREN,
                expected_literal: "(",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "five",
            },
            TestTokens {
                expected_type: TokenType::COMMA,
                expected_literal: ",",
            },
            TestTokens {
                expected_type: TokenType::IDENT,
                expected_literal: "ten",
            },
            TestTokens {
                expected_type: TokenType::RPAREN,
                expected_literal: ")",
            },
            TestTokens {
                expected_type: TokenType::SEMICOLON,
                expected_literal: ";",
            },
        ];
        let mut lexer = Lexer::new(input);

        for (i, tt) in EXPECTED.iter().enumerate() {
            let tok = lexer.next_token();

            if tok.r#type != tt.expected_type {
                panic!(
                    "{:?} - token_type wrong. expected = {:?}, got = {:?}",
                    EXPECTED[i], tt.expected_type, tok.r#type
                )
            }
            if tok.literal != tt.expected_literal {
                panic!(
                    "{:?} - token_type wrong. expected = {:?}, got = {:?}",
                    EXPECTED[i], tt.expected_literal, tok.literal
                )
            }
        }
    }
}
