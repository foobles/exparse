#[derive(Copy, Clone, Debug)]
pub struct ParseState<'a> {
    text: &'a str,
    row: usize,
    col: usize,
}

impl<'a> ParseState<'a> {
    pub fn new(text: &'a str) -> Self {
        ParseState {
            text,
            row: 0,
            col: 0,
        }
    }

    pub fn make_error(self, ty: ParseErrorType) -> ParseError {
        ParseError {
            ty,
            row: self.row,
            col: self.col,
        }
    }

    pub fn parse<P: Parser>(&mut self, parser: P) -> Result<P::Output, ParseError> {
        parser.parse(self)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ParseError {
    ty: ParseErrorType,
    row: usize,
    col: usize,
}

impl ParseError {
    pub fn row(self) -> usize {
        self.row
    }

    pub fn col(self) -> usize {
        self.col
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ParseErrorType {
    EndOfStream,
    NoParse
}

pub trait Parser: Sized {
    type Output;

    fn parse(&self, state: &mut ParseState) -> Result<Self::Output, ParseError>;

    fn or<P: Parser>(self, other: P) -> Or<Self, P> {
        Or {
            first: self,
            second: other
        }
    }

    fn or_more(self) -> OrMore<Self> {
        OrMore(self)
    }

    fn any_number(self) -> AnyNumber<Self> {
        AnyNumber(self)
    }
}

impl<P: Parser> Parser for &P {
    type Output = P::Output;

    fn parse(&self, state: &mut ParseState) -> Result<Self::Output, ParseError> {
        P::parse(*self, state)
    }
}

pub struct AnyChar;

pub fn any_char() -> AnyChar {
    AnyChar
}

impl Parser for AnyChar {
    type Output = char;

    fn parse(&self, state: &mut ParseState) -> Result<char, ParseError> {
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
            .ok_or(state.make_error(ParseErrorType::EndOfStream))
    }
}

pub struct SkipWhiteSpace;

pub fn skip_whitespace() -> SkipWhiteSpace {
    SkipWhiteSpace
}

impl Parser for SkipWhiteSpace {
    type Output = ();

    fn parse(&self, state: &mut ParseState) -> Result<(), ParseError> {
        let mut cur = *state;
        while let Ok(c) = cur.parse(any_char()) {
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

impl Parser for Char {
    type Output = char;

    fn parse(&self, state: &mut ParseState) -> Result<char, ParseError> {
        let mut cur = *state;
        cur.parse(any_char()).and_then(|x| {
            if x == self.0 {
                *state = cur;
                Ok(x)
            } else {
                Err(state.make_error(ParseErrorType::NoParse))
            }
        })
    }
}

pub struct Or<P, Q> {
    first: P,
    second: Q
}

impl<P, Q> Parser for Or<P, Q>
where
    P: Parser,
    Q: Parser<Output=P::Output>
{
    type Output = P::Output;

    fn parse(&self, state: &mut ParseState) -> Result<Self::Output, ParseError> {
        state.parse(&self.first)
            .or_else(|_| state.parse(&self.second))
    }
}

pub struct AnyNumber<P>(P);

impl<P: Parser> Parser for AnyNumber<P> {
    type Output = Vec<P::Output>;

    fn parse(&self, state: &mut ParseState) -> Result<Self::Output, ParseError> {
        let mut ret = Vec::new();
        while let Ok(x) = state.parse(&self.0) {
            ret.push(x);
        }
        Ok(ret)
    }
}


pub struct OrMore<P>(P);

impl<P: Parser> Parser for OrMore<P> {
    type Output = Vec<P::Output>;

    fn parse(&self, state: &mut ParseState) -> Result<Self::Output, ParseError> {
        let mut ret = vec![state.parse(&self.0)?];
        while let Ok(x) = state.parse(&self.0) {
           ret.push(x);
        }
        Ok(ret)
    }
}

pub fn digit() -> impl Parser<Output=char> {
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

impl Parser for I32 {
    type Output = i32;

    fn parse(&self, state: &mut ParseState) -> Result<i32, ParseError> {
        state.parse(skip_whitespace())?;
        let sign= match state.parse(char('-')) {
            Ok(_) => -1,
            Err(_) => 1
        };
        let mut ret = 0;
        for d in state.parse(digit().or_more())? {
            ret *= 10;
            ret += d.to_digit(10).unwrap() as i32;
        }
        Ok(ret * sign)
    }
}

