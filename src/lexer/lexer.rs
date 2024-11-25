use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'static str,
    // points to the char in the input that corresponds to the ch
    position: usize,
    // points to the next char in the input
    read_position: usize,
    ch: Option<&'a [u8]>,
}

impl Lexer<'_> {
    fn new(input: &'static str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };

        lexer.init();
        lexer
    }

    fn init(&mut self) {
        self.read_char();
    }

    fn next_token(&mut self) -> Token {
        let tok = match self.ch {
            Some(b"=") => Token {
                literal: "=",
                r#type: TokenType::ASSIGN,
            },
            Some(b";") => Token {
                literal: ";",
                r#type: TokenType::SEMICOLON,
            },
            Some(b"(") => Token {
                literal: "(",
                r#type: TokenType::LPARAN,
            },
            Some(b")") => Token {
                literal: ")",
                r#type: TokenType::RPARAN,
            },
            Some(b"{") => Token {
                literal: "{",
                r#type: TokenType::LBRACE,
            },
            Some(b"}") => Token {
                literal: "}",
                r#type: TokenType::RBRACE,
            },
            Some(b",") => Token {
                literal: ",",
                r#type: TokenType::COMMA,
            },
            Some(b"+") => Token {
                literal: "+",
                r#type: TokenType::PLUS,
            },
            None => Token {
                literal: "/0",
                r#type: TokenType::EOF,
            },
            Some(char) => Token {
                // TODO: clean up this leak. Program probably ends at this point so maybe
                // leaking memory isn't the worst thing to do here.
                literal: Box::leak(Box::new(String::from_utf8(char.to_vec()).unwrap())),
                r#type: TokenType::ILLEGAL,
            },
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None
        } else {
            self.ch = Some(self.input.as_bytes()[self.read_position]);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[derive(Debug)]
    struct TestTokens {
        expected_type: TokenType,
        expected_literal: char,
    }

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        const EXPECTED: [TestTokens; 9] = [
            TestTokens {
                expected_type: TokenType::ASSIGN,
                expected_literal: '=',
            },
            TestTokens {
                expected_literal: '+',
                expected_type: TokenType::PLUS,
            },
            TestTokens {
                expected_type: TokenType::LPARAN,
                expected_literal: '(',
            },
            TestTokens {
                expected_literal: ')',
                expected_type: TokenType::RPARAN,
            },
            TestTokens {
                expected_type: TokenType::LBRACE,
                expected_literal: '{',
            },
            TestTokens {
                expected_type: TokenType::RBRACE,
                expected_literal: '}',
            },
            TestTokens {
                expected_type: TokenType::COMMA,
                expected_literal: ',',
            },
            TestTokens {
                expected_type: TokenType::SEMICOLON,
                expected_literal: ';',
            },
            TestTokens {
                expected_literal: '\0',
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

        const EXPECTED: [TestTokens; 10] = [
            TestTokens {
                expected_type: TokenType::LET,
                expected_literal: "let"
            }
        ]
    }
}
