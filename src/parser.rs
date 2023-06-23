use crate::ast;
use crate::lexer::Lexer;
use crate::lexer::Token;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn token_precedences(tok: &Token) -> Precedence {
        match tok {
            Token::Eq => Precedence::Equals,
            Token::Neq => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

type ParseError = String;
type PrefixFn = fn(parser: &mut Parser<'_>) -> Result<ast::Expression, ParseError>;
type InfixFn =
    fn(parser: &mut Parser<'_>, left: ast::Expression) -> Result<ast::Expression, ParseError>;

pub struct Parser<'a> {
    l: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut l: Lexer<'a>) -> Self {
        let cur_token = l.next_token();
        let peek_token = l.next_token();
        Self {
            l,
            cur_token,
            peek_token,
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, Vec<ParseError>> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };
        let mut errors = Vec::new();
        while self.cur_token.clone() != Token::EOF {
            let statement = self.parse_statement();
            match statement {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => errors.push(err),
            }
            self.next_token();
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn expect_peek(&mut self, token: Token) -> Result<(), ParseError> {
        if self.peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(format!(
                "Expected next token to be {:?} but got {:?}",
                token, self.peek_token,
            ))
        }
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        match (&token, &self.peek_token) {
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Int(_), Token::Int(_)) => true,
            _ => token == &self.peek_token,
        }
    }

    fn cur_token_is(&self, token: Token) -> bool {
        match (&token, &self.cur_token) {
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Int(_), Token::Int(_)) => true,
            _ => token == self.peek_token,
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        let name = match &self.peek_token {
            Token::Ident(name) => name.to_string(),
            _ => {
                return Err(format!(
                    "Expected indentifier but got {:?}",
                    self.peek_token
                ));
            }
        };
        self.next_token();
        Ok(name)
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, ParseError> {
        let name = self.expect_ident()?;

        self.expect_peek(Token::Assign)?;

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(ast::Statement::Let(ast::LetStatement { name, value }))
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement, ParseError> {
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(ast::Statement::Return(ast::ReturnStatement { value }))
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement, ParseError> {
        let expr = self.parse_expression(Precedence::Lowest);
        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }
        match expr {
            Ok(expr) => Ok(ast::Statement::Expression(ast::ExpressionStatement {
                expression: expr,
            })),
            Err(err) => Err(err),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, ParseError> {
        let mut left_exp;

        if let Some(f) = self.prefix_fn() {
            left_exp = f(self)?;
        } else {
            return Err(format!("No prefix fn found for {:?}", self.cur_token));
        }

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            match self.infix_fn() {
                Some(f) => {
                    self.next_token();
                    left_exp = f(self, left_exp)?;
                }
                None => return Ok(left_exp),
            }
        }

        Ok(left_exp)
    }

    pub fn parse_identifier(parser: &mut Parser<'_>) -> Result<ast::Expression, ParseError> {
        if let Token::Ident(ref name) = parser.cur_token {
            return Ok(ast::Expression::Identifier(name.to_string()));
        }
        Err(format!(
            "Error when parsing {:?} as identifier",
            parser.cur_token
        ))
    }

    pub fn parse_integer(parser: &mut Parser<'_>) -> Result<ast::Expression, ParseError> {
        if let Token::Int(val) = parser.cur_token {
            return Ok(ast::Expression::Int(val));
        }
        Err(format!(
            "Error when parsing {:?} as identifier",
            parser.cur_token
        ))
    }

    pub fn parse_prefix_expression(parser: &mut Parser<'_>) -> Result<ast::Expression, ParseError> {
        let operator = parser.cur_token.clone();
        parser.next_token();
        let right = parser.parse_expression(Precedence::Prefix)?;
        Ok(ast::Expression::Prefix(Box::new(ast::PrefixExpression {
            operator,
            right,
        })))
    }

    pub fn parse_infix_expression(
        parser: &mut Parser<'_>,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParseError> {
        let operator = parser.cur_token.clone();
        let precedence = parser.cur_precedence();
        parser.next_token();
        let right = parser.parse_expression(precedence)?;
        Ok(ast::Expression::Infix(Box::new(ast::InfixExpression {
            left,
            operator,
            right,
        })))
    }

    fn prefix_fn(&mut self) -> Option<PrefixFn> {
        match self.cur_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    fn infix_fn(&mut self) -> Option<InfixFn> {
        match self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Eq
            | Token::Neq
            | Token::Lt
            | Token::Gt => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn peek_precedence(&mut self) -> Precedence {
        Precedence::token_precedences(&self.peek_token)
    }

    fn cur_precedence(&mut self) -> Precedence {
        Precedence::token_precedences(&self.cur_token)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;

    fn setup(input: &str, stmt_count: usize) -> ast::Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let prog = p.parse_program().unwrap();

        if stmt_count != 0 && prog.statements.len() != stmt_count {
            panic!(
                "expected 1 statement for '{}' but got {:?}",
                input, prog.statements
            )
        }

        prog
    }

    #[test]
    fn let_statement() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";

        let prog = setup(input, 3);

        let tests = vec!["x", "y", "foobar"];

        let mut itr = prog.statements.iter();

        for t in tests {
            match itr.next().unwrap() {
                ast::Statement::Let(ref l) => {
                    assert_eq!(l.name, t);
                }
                _ => panic!("unknown node"),
            }
        }
    }
}
