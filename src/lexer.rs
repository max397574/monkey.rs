use std::iter::Peekable;
use std::str::Chars;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(String),
    Int(i64),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,
    Eq,
    Neq,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn read_identifier(&mut self, c: char) -> String {
        let mut ident = String::new();
        ident.push(c);
        while let Some(ch) = self.input.peek() {
            if is_letter(*ch) {
                ident.push(self.read_char().unwrap())
            } else {
                break;
            }
        }
        ident
    }

    fn read_number(&mut self, c: char) -> String {
        let mut number = String::new();
        number.push(c);
        while let Some(ch) = self.input.peek() {
            if ch.is_ascii_digit() {
                number.push(self.read_char().unwrap())
            } else {
                break;
            }
        }
        number
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.input.peek() {
            if ch.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.read_char() {
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('/') => Token::Slash,
            Some('*') => Token::Asterisk,
            Some('!') => {
                if self.input.peek().is_some_and(|ch| ch == &'=') {
                    self.read_char();
                    Token::Neq
                } else {
                    Token::Bang
                }
            }
            Some('<') => Token::Lt,
            Some('>') => Token::Gt,
            Some('=') => {
                if self.input.peek().is_some_and(|ch| ch == &'=') {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some(ch) => {
                if is_letter(ch) {
                    let identifier = self.read_identifier(ch);
                    match identifier.as_str() {
                        "fn" => Token::Function,
                        "let" => Token::Let,
                        "true" => Token::True,
                        "false" => Token::False,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        _ => Token::Ident(identifier),
                    }
                } else if ch.is_ascii_digit() {
                    let number = self.read_number(ch);
                    Token::Int(number.parse::<i64>().unwrap())
                } else {
                    Token::Illegal
                }
            }
            None => Token::EOF,
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token {
            Token::EOF => None,
            tkn => Some(tkn),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_tokens() {
        let mut l = Lexer::new("=+(){},;");
        let tests = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
        ];
        for token in tests.iter() {
            assert_eq!(token, &l.next().unwrap());
        }
    }

    #[test]
    fn complex_program() {
        // TODO: fix tests passing even though whitespace isn't removed yet
        // let mut same = true;
        let mut l = Lexer::new(
            "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
",
        );
        let tests = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
        ];
        for token in tests.iter() {
            assert_eq!(token, &l.next().unwrap());
        }
    }
}
