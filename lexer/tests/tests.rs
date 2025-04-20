use lexer::*;
use token::Token;

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
            true && false;
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
        Token::True,
        Token::And,
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
