use crate::lexer::LexerError;
use crate::lexer::Token;
use crate::lexer::TokenVariant::{self,};
use crate::Lexer;

use std::error;
use std::fmt;

#[derive(Debug)]
pub enum ParseError {
	LexerError(LexerError),
	UnexpectedToken {
		expected: Vec<TokenVariant>,
		found: Token,
	},
}

impl ParseError {
	fn unexpected_token(expected: &[TokenVariant], found: Token) -> ParseError {
		ParseError::UnexpectedToken {
			expected: expected.to_vec(),
			found,
		}
	}
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl error::Error for ParseError {}

#[derive(Debug)]
pub struct StructMember {
	name: Token, // Identifier
	variant: Token,
}

#[derive(Debug)]
pub struct StructDeclaration {
	name: Token,
	members: Vec<StructMember>,
}

#[derive(Debug)]
pub struct FuntionDeclaration {
	name: Token,
	params: Vec<Token>,
	result: Option<Token>,
}

#[derive(Debug)]
pub struct ParsedData {
	struct_declarations: Vec<StructDeclaration>,
	function_declarations: Vec<FuntionDeclaration>,
}

pub struct Parser<'a> {
	lexer: Lexer<'a>,
	current: Token,
}

impl<'a> Parser<'a> {
	pub fn parse(contents: &'a str) -> Result<ParsedData, ParseError> {
		let mut lexer = Lexer::new(contents);
		let current = lexer.next().map_err(ParseError::LexerError)?;

		let mut parser = Parser { lexer, current };

		let mut struct_declarations = Vec::with_capacity(1024);
		let mut function_declarations = Vec::with_capacity(1024);

		loop {
			match parser.current.variant {
				TokenVariant::Struct => {
					struct_declarations.push(parser.struct_declaration()?);
				}
				TokenVariant::Fn => {
					parser.function_declaration()?;
				}
				TokenVariant::LineComment | TokenVariant::BlockComment => {
					parser.next()?;
				}
				TokenVariant::EndOfFile => break,
				_ => {
					return Err(ParseError::unexpected_token(
						&[TokenVariant::Struct, TokenVariant::Fn],
						parser.current.clone(),
					))
				}
			}
		}

		Ok(ParsedData {
			struct_declarations,
			function_declarations,
		})
	}

	fn next(&mut self) -> Result<Token, ParseError> {
		let result = self.current.clone();
		self.current = self.lexer.next().map_err(ParseError::LexerError)?;
		Ok(result)
	}

	#[must_use]
	fn accept(&mut self, variant: TokenVariant) -> Option<Result<Token, ParseError>> {
		if self.current.variant == variant {
			Some(self.next())
		} else {
			None
		}
	}

	fn expect(&mut self, variant: TokenVariant) -> Result<Token, ParseError> {
		match self.accept(variant) {
			Some(result) => result,
			None => Err(ParseError::unexpected_token(
				&[variant],
				self.current.clone(),
			)),
		}
	}

	fn struct_declaration(&mut self) -> Result<StructDeclaration, ParseError> {
		self.expect(TokenVariant::Struct)?;
		let struct_name = self.expect(TokenVariant::Identifier)?;
		self.expect(TokenVariant::LeftCurly)?;

		let mut members = Vec::with_capacity(64);

		while let Some(result) = self.accept(TokenVariant::Identifier) {
			let member_name = result?;

			self.expect(TokenVariant::Colon)?;
			let variant_name = self.expect(TokenVariant::Identifier)?;

			members.push(StructMember {
				name: member_name,
				variant: variant_name,
			});

			if let Some(result) = self.accept(TokenVariant::Comma) {
				result?;
			} else {
				break;
			}
		}

		self.expect(TokenVariant::RightCurly)?;
		Ok(StructDeclaration {
			name: struct_name,
			members,
		})
	}

	fn function_declaration(&mut self) -> Result<(), ParseError> {
		self.expect(TokenVariant::Fn)?;
		self.expect(TokenVariant::Identifier)?;
		self.expect(TokenVariant::OpenParen)?;

		while let Some(result) = self.accept(TokenVariant::Identifier) {
			result?;

			self.expect(TokenVariant::Colon)?;
			self.expect(TokenVariant::Identifier)?;

			if let Some(result) = self.accept(TokenVariant::Comma) {
				result?;
			} else {
				break;
			}
		}

		self.expect(TokenVariant::CloseParen)?;
		if let Some(result) = self.accept(TokenVariant::Arrow) {
			result?;
			self.expect(TokenVariant::Identifier)?;
		}

		if self.current.variant == TokenVariant::LeftCurly {
			self.statement()
		} else {
			Err(ParseError::unexpected_token(
				&[TokenVariant::LeftCurly],
				self.current.clone(),
			))
		}
	}

	fn statement(&mut self) -> Result<(), ParseError> {
		match self.current.variant {
			TokenVariant::Let => {
				self.expect(TokenVariant::Let)?;
				if let Some(result) = self.accept(TokenVariant::Mut) {
					result?;
				}

				self.expect(TokenVariant::Identifier)?;

				self.expect(TokenVariant::Equal)?;
				self.expect(TokenVariant::Identifier)?;
				self.expect(TokenVariant::LeftCurly)?;

				while let Some(result) = self.accept(TokenVariant::Identifier) {
					result?;
					self.expect(TokenVariant::Colon)?;
					self.expression()?;
					if let Some(result) = self.accept(TokenVariant::Comma) {
						result?;
					} else {
						break;
					}
				}

				self.expect(TokenVariant::RightCurly)?;
				Ok(())
			}
			TokenVariant::Fn => self.function_declaration(),
			// TokenVariant::If => {
			// 	self.expect(TokenVariant::If)?;
			// 	self.condition()?;
			// }
			TokenVariant::LeftCurly => {
				self.expect(TokenVariant::LeftCurly);
				loop {
					self.statement()?;

					if let Some(result) = self.accept(TokenVariant::SemiColon) {
						result?;
					} else {
						break;
					}
				}
				self.expect(TokenVariant::RightCurly);
				Ok(())
			}
			_ => Err(ParseError::unexpected_token(&[], self.current.clone())),
		}
	}

	// fn condition(&mut self) -> Result<(), ParseError> {}

	fn expression(&mut self) -> Result<(), ParseError> {
		match self.current.variant {
			TokenVariant::Number | TokenVariant::Char | TokenVariant::String => {
				self.next().map(|_| ())
			}
			_ => Err(ParseError::unexpected_token(&[], self.current.clone())),
		}
	}

	// fn term(&mut self) -> Result<(), ParseError> {}

	// fn factor(&mut self) -> Result<(), ParseError> {
	// 	match self.current.variant {
	// 		_ => Err(ParseError::unexpected_token(&[], found)),
	// 	}
	// }
}
