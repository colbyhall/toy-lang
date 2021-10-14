use std::fs::OpenOptions;

use crate::lexer::LexerError;
use crate::lexer::Token;
use crate::lexer::TokenVariant;
use crate::Lexer;

pub enum ParseError<'a> {
	LexerError(LexerError),
	UnexpectedToken(Token<'a>),
}

pub struct Parser<'a> {
	lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
	pub fn parse(contents: &'a str) -> Result<(), ParseError> {
		let mut parser = Parser {
			lexer: Lexer::new(contents),
		};

		Ok(())
	}

	fn accept(&mut self, variant: TokenVariant) -> Option<Result<(), ParseError>> {
		if self.current.variant == variant {
			Some(self.next())
		} else {
			None
		}
	}

	fn expect(&mut self, variant: TokenVariant) -> Result<(), ParseError> {
		if self.accept(variant).is_some() {
			Ok(())
		} else {
			Err(ParseError::UnexpectedToken(self.current.clone()))
		}
	}
}
