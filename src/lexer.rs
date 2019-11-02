use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug)]
pub struct LexState<'a> {
    text: &'a str,
    row: usize,
    col: usize,
}

impl<'a> LexState<'a> {
    pub fn new(text: &'a str) -> Self {
        LexState {
            text,
            row: 0,
            col: 0,
        }
    }

    pub fn is_done(self) -> bool {
        self.text.is_empty()
    }

    pub fn make_error(self, ty: LexErrorType) -> LexError {
        LexError {
            ty,
            row: self.row,
            col: self.col,
        }
    }

    pub fn lex<P: Lexer>(&mut self, parser: P) -> Result<P::Output, LexError> {
        let mut copied_state = *self;
        let ret = parser.lex(&mut copied_state)?;
        *self = copied_state;
        Ok(ret)
    }

    pub fn row(self) -> usize {
        self.row
    }

    pub fn col(self) -> usize {
        self.col
    }
}

#[derive(Copy, Clone, Debug)]
pub struct LexError {
    ty: LexErrorType,
    row: usize,
    col: usize,
}

impl Display for LexError {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        write!(formatter, "Error at {}:{}: {:?}", self.row, self.col, self.ty)
    }
}

impl std::error::Error for LexError {}

#[derive(Copy, Clone, Debug)]
pub enum LexErrorType {
    EndOfStream,
    NoParse
}

pub trait Lexer: Sized {
    type Output;

    fn lex(&self, state: &mut LexState) -> Result<Self::Output, LexError>;

    fn or<P>(self, other: P) -> Or<Self, P>
    where
        P: Lexer<Output = Self::Output>
    {
        Or {
            first: self,
            second: other
        }
    }

    fn or_more(self) -> OrMore<Self> {
        OrMore(self)
    }

    fn map<F, T>(self, func: F) -> Map<Self, F>
    where
        F: Fn(Self::Output) -> T
    {
        Map {
            parser: self,
            func
        }
    }
}

impl<P: Lexer> Lexer for &P {
    type Output = P::Output;

    fn lex(&self, state: &mut LexState) -> Result<Self::Output, LexError> {
        P::lex(*self, state)
    }
}

pub struct AnyChar;

pub fn any_char() -> AnyChar {
    AnyChar
}

impl Lexer for AnyChar {
    type Output = char;

    fn lex(&self, state: &mut LexState) -> Result<char, LexError> {
        state.text
            .chars()
            .next()
            .map(|c| {
                state.text = &state.text[c.len_utf8()..];
                if c == '\n' {
                    state.row += 1;
                } else {
                    state.col += 1;
                }
                c
            })
            .ok_or(state.make_error(LexErrorType::EndOfStream))
    }
}

pub struct SkipWhiteSpace;

pub fn skip_whitespace() -> SkipWhiteSpace {
    SkipWhiteSpace
}

impl Lexer for SkipWhiteSpace {
    type Output = ();

    fn lex(&self, state: &mut LexState) -> Result<(), LexError> {
        let mut cur = *state;
        while let Ok(c) = cur.lex(any_char()) {
            if !c.is_whitespace() {
                return Ok(());
            }
            *state = cur;
        }
        Ok(())
    }
}

pub struct Char(char);

pub fn char(c: char) -> Char {
    Char(c)
}

impl Lexer for Char {
    type Output = char;

    fn lex(&self, state: &mut LexState) -> Result<char, LexError> {
        state.lex(any_char()).and_then(|x| {
            if x == self.0 {
                Ok(x)
            } else {
                Err(state.make_error(LexErrorType::NoParse))
            }
        })
    }
}

pub struct Or<P, Q> {
    first: P,
    second: Q
}

impl<P, Q> Lexer for Or<P, Q>
where
    P: Lexer,
    Q: Lexer<Output=P::Output>
{
    type Output = P::Output;

    fn lex(&self, state: &mut LexState) -> Result<Self::Output, LexError> {
        state.lex(&self.first)
            .or_else(|_| state.lex(&self.second))
    }
}



pub struct OrMore<P>(P);

impl<P: Lexer> Lexer for OrMore<P> {
    type Output = Vec<P::Output>;

    fn lex(&self, state: &mut LexState) -> Result<Self::Output, LexError> {
        let mut ret = vec![state.lex(&self.0)?];
        while let Ok(x) = state.lex(&self.0) {
           ret.push(x);
        }
        Ok(ret)
    }
}

pub struct Map<P, F> {
    parser: P,
    func: F
}

impl<P, F, T> Lexer for Map<P, F>
where
    P: Lexer,
    F: Fn(P::Output) -> T
{
    type Output = T;

    fn lex(&self, state: &mut LexState) -> Result<Self::Output, LexError> {
        state.lex(&self.parser).map(&self.func)
    }
}

pub fn digit() -> impl Lexer<Output=char> {
    char('0')
        .or(char('1'))
        .or(char('2'))
        .or(char('3'))
        .or(char('4'))
        .or(char('5'))
        .or(char('6'))
        .or(char('7'))
        .or(char('8'))
        .or(char('9'))
}

pub struct I32;

pub fn i32() -> I32 {
    I32
}

impl Lexer for I32 {
    type Output = i32;

    fn lex(&self, state: &mut LexState) -> Result<i32, LexError> {
        let sign= match state.lex(char('-')) {
            Ok(_) => -1,
            Err(_) => 1
        };
        let mut ret = 0;
        for d in state.lex(digit().or_more())? {
            ret *= 10;
            ret += d.to_digit(10).unwrap() as i32;
        }
        Ok(ret * sign)
    }
}

pub fn from_fn<F, T>(func: F) -> FromFn<F>
where
    F: Fn(&mut LexState) -> Result<T, LexError>
{
    FromFn(func)
}

pub struct FromFn<F>(F);

impl<F, T> Lexer for FromFn<F>
where
    F: Fn(&mut LexState) -> Result<T, LexError>
{
    type Output = T;

    fn lex(&self, state: &mut LexState) -> Result<T, LexError> {
        self.0(state)
    }
}