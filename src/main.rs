mod lexer;
use lexer::Lexer;
use crate::lexer::LexState;
use std::fmt::{Display, Formatter};
use std::io::{self, BufReader, BufRead};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum TokenType {
    LParen,
    RParen,
    Num(i32),
    Add,
    Sub,
    Mul,
    Div,
    Eof
}

impl Display for TokenType {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        use TokenType::*;
        write!(formatter, "{}", match self {
            LParen => '(',
            RParen => ')',
            Add => '+',
            Sub => '-',
            Mul => '*',
            Div => '/',
            Num(n) => return write!(formatter, "{}", n.to_string()),
            Eof => return write!(formatter, "EOF")
        })
    }
}

#[derive(Copy, Clone, Debug)]
struct Token {
    ty: TokenType,
    row: usize,
    col: usize
}

impl Token {
    fn new(ty: TokenType, state: LexState) -> Self {
        Token {
            ty,
            row: state.row(),
            col: state.col()
        }
    }

    fn make_error(self, message: String) -> Error {
        Error {
            message,
            row: self.row,
            col: self.col
        }
    }

    fn ty(self) -> TokenType {
        self.ty
    }
}

fn lex_arithmetic() -> impl Lexer<Output = Vec<Token>> {
    lexer::from_fn(|state| {
        use TokenType::*;
        let mut ret = vec![];
        state.lex(lexer::skip_whitespace()).unwrap();
        while !state.is_done() {
            let s = *state;
            ret.push(state.lex(
                lexer::i32().map(|n| Token::new(Num(n), s))
                    .or(lexer::char('+').map(|_| Token::new(Add, s)))
                    .or(lexer::char('-').map(|_| Token::new(Sub, s)))
                    .or(lexer::char('*').map(|_| Token::new(Mul, s)))
                    .or(lexer::char('/').map(|_| Token::new(Div, s)))
                    .or(lexer::char('(').map(|_| Token::new(LParen, s)))
                    .or(lexer::char(')').map(|_| Token::new(RParen, s)))
            )?);
            state.lex(lexer::skip_whitespace()).unwrap();
        }
        ret.push(Token::new(Eof, *state));
        Ok(ret)
    })
}

#[derive(Copy, Clone, Debug)]
struct TokenStream<'a> {
    toks: &'a [Token]
}

macro_rules! expect {
    ($tokens:expr, $tok:pat) => {{
        let cur = $crate::TokenStream::cur(*$tokens);
        match cur.ty() {
            r@$tok => {
                $crate::TokenStream::advance($tokens);
                ::std::result::Result::Ok(r)
            }
            x => ::std::result::Result::Err(cur.make_error(::std::format!("expected {}, got '{}'", stringify!($tok), x)))
        }
    }}
}

#[derive(Clone, Debug)]
struct Error {
    message: String,
    row: usize,
    col: usize
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        write!(formatter, "Error at {}:{}: {}", self.row, self.col, self.message)
    }
}

impl std::error::Error for Error {}

impl TokenStream<'_> {

    fn advance(&mut self) {
        if self.toks[0].ty != TokenType::Eof {
            self.toks = &self.toks[1..];
        }
    }

    fn cur(self) -> Token {
        self.toks[0]
    }



    fn expr(&mut self) -> Result<i32, Error> {
        let mut ret = self.term()?;
        loop {
            match self.cur().ty() {
                t @ TokenType::Add | t @ TokenType::Sub => {
                    self.advance();
                    let rhs = self.term()?;
                    if t == TokenType::Add {
                        ret += rhs;
                    } else {
                        ret -= rhs;
                    }
                },
                _ => break Ok(ret)
            }
        }
    }

    fn term(&mut self) -> Result<i32, Error> {
        let mut ret = self.value()?;
        loop {
            match self.cur().ty() {
                t @ TokenType::Mul | t @ TokenType::Div => {
                    self.advance();
                    let rhs = self.value()?;
                    if t == TokenType::Mul {
                        ret *= rhs;
                    } else {
                        ret /= rhs;
                    }
                },
                _ => break Ok(ret)
            }
        }
    }

    fn value(&mut self) -> Result<i32, Error> {
        Ok(match self.cur().ty() {
            TokenType::Num(n)  => {
                self.advance();
                n
            },
            TokenType::LParen => {
                self.advance();
                let ret = self.expr()?;
                expect!(self, TokenType::RParen)?;
                ret
            },
            _ => return Err(self.cur().make_error(format!("Expected number or parenthesized expression, got '{}'", self.cur().ty())))
        })
    }

    fn statement(&mut self) -> Result<i32, Error> {
        let ret = self.expr()?;
        expect!(self, TokenType::Eof)?;
        Ok(ret)
    }
}




fn main() -> io::Result<()> {
    for line in BufReader::new(io::stdin()).lines() {
        let line = line?;
        let mut lex_state = LexState::new(&line);
        let token_stream = lex_state.lex(lex_arithmetic());
        match token_stream {
            Ok(tokens) => {
                let mut ts = TokenStream {toks: &tokens};
                match ts.statement() {
                    Ok(n) => println!("{}", n),
                    Err(e) => eprintln!("{}", e)
                }
            }
            Err(e) => eprintln!("{:?}", e)
        }
    }
    Ok(())
}
