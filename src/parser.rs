use crate::Lexer;

pub struct Parser<'a> {
	lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
	pub fn parse(contents: &'a str) -> Option<()> {
		let mut parser = Parser {
			lexer: Lexer::new(contents),
		};

		Some(())
	}
}
