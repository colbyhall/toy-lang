use std::iter::Iterator;
use std::str::Chars;
use std::error::Error;

#[derive(Debug)]
pub enum Keyword {
    If,
    Let,
    While,
    Fn,
}

#[derive(Debug)]
pub enum Seperator {
    LeftCurly,
    RightCurly,
    Colon,
    SemiColon,
    Comma
}

#[derive(Debug)]
pub enum Operator {
    Equal,
    LessThan,
    GreaterThan,
    Bang,
}

#[derive(Debug)]
pub enum Literal<'a> {
    Char(char),
    String(&'a str),
    Int{
        signed: bool,
        value: u64,
    }
}

#[derive(Debug)]
pub enum Token<'a> {
    Identifier(&'a str),
    Keyword(Keyword),
    Seperator(Seperator),
    Operator(Operator),
    Literal(Literal<'a>),
    Comment,
}



#[derive(Debug)]
pub struct Tokenizer<'a> {
    chars: Chars<'a>,

    line: i32,
    col: i32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(contents: &'a str) -> Self {
        Self {
            chars: contents.chars(),
            
            line: 0,
            col: 0,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        match c {

            _ => todo!()
        }
    }
}