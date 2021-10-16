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
		#[cfg(debug_assertions)]
		panic!(
			"{:?}",
			ParseError::UnexpectedToken {
				expected: expected.to_vec(),
				found,
			}
		);

		#[cfg(not(debug_assertions))]
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
pub struct Ast {
	pub body: Body,
}

#[derive(Debug)]
pub struct Body {
	pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
	Definition {
		name: Token,
		mutable: bool,
		variant: Option<Token>,
		value: Expression,
	},
	Assignment {
		name: Expression, // Variable
		value: Expression,
	},
	StructDeclaration {
		name: Token,
		members: Vec<Declaration>,
	},
	FuntionDeclaration {
		name: Token,
		params: Vec<Declaration>,
		result: Option<Token>,
		body: Body,
	},
	Expression(Expression),
	Body(Body),
}

#[derive(Debug)]
pub struct Declaration {
	pub name: Token,
	pub variant: Token,
}

#[derive(Debug)]
pub enum Expression {
	Branch {
		if_branch: Box<Branch>,
		else_if_branches: Vec<Branch>,
		else_body: Option<Body>,
	},
	FunctionCall {
		name: Token,
		params: Vec<Expression>,
	},
	Constant {
		token: Token,
	},
	StructLiteral {
		variant: Token,
		members: Vec<StructLiteralDefinition>,
	},
	Variable {
		name: Token,
		accesses: Vec<Token>,
	},
}

#[derive(Debug)]
pub struct Branch {
	pub condition: Expression,
	pub body: Body,
}

#[derive(Debug)]
pub struct StructLiteralDefinition {
	name: Token,
	expression: Expression,
}

pub struct Parser<'a> {
	lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
	pub fn parse(contents: &'a str) -> Result<Ast, ParseError> {
		let mut parser = Parser {
			lexer: Lexer::new(contents).ignore_comments(),
		};

		let mut statements = Vec::with_capacity(1024);

		loop {
			match parser.peek()?.variant {
				TokenVariant::Struct => {
					statements.push(parser.struct_declaration()?);
				}
				TokenVariant::Fn => {
					statements.push(parser.function_declaration()?);
				}
				TokenVariant::EndOfFile => break,
				_ => {
					return Err(ParseError::unexpected_token(
						&[TokenVariant::Struct, TokenVariant::Fn],
						parser.next()?,
					))
				}
			}
		}

		Ok(Ast {
			body: Body { statements },
		})
	}

	fn next(&mut self) -> Result<Token, ParseError> {
		self.lexer.next().map_err(ParseError::LexerError)
	}

	fn peek(&mut self) -> Result<Token, ParseError> {
		self.lexer.peek().map_err(ParseError::LexerError)
	}

	fn accept(&mut self, variant: TokenVariant) -> Result<Option<Token>, ParseError> {
		if self.peek()?.variant == variant {
			self.next().map(Some)
		} else {
			Ok(None)
		}
	}

	fn expect(&mut self, variant: TokenVariant) -> Result<Token, ParseError> {
		if let Some(token) = self.accept(variant)? {
			Ok(token)
		} else {
			Err(ParseError::unexpected_token(&[variant], self.next()?))
		}
	}

	fn body(&mut self) -> Result<Body, ParseError> {
		self.expect(TokenVariant::LeftCurly)?;
		let mut statements = Vec::with_capacity(1024);
		loop {
			let statement = self.statement()?;
			match statement {
				// These statements require semicolons
				Statement::Assignment { .. } | Statement::Definition { .. } => {
					self.expect(TokenVariant::SemiColon)?;
				}
				// These do not. Well expressions do but they handle that themselves
				_ => {}
			}
			statements.push(statement);
			if self.accept(TokenVariant::RightCurly)?.is_some() {
				break;
			}
		}
		Ok(Body { statements })
	}

	fn statement(&mut self) -> Result<Statement, ParseError> {
		match self.peek()?.variant {
			TokenVariant::Let => {
				self.expect(TokenVariant::Let)?;
				let mutable = self.accept(TokenVariant::Mut)?.is_some();
				let name = self.expect(TokenVariant::Identifier)?;

				let variant = None; // TODO

				self.expect(TokenVariant::Equal)?;
				let expression = self.expression()?;
				Ok(Statement::Definition {
					name,
					mutable,
					variant,
					value: expression,
				})
			}
			TokenVariant::Fn => self.function_declaration(),
			TokenVariant::Struct => self.struct_declaration(),
			TokenVariant::LeftCurly => self.body().map(Statement::Body),
			_ => {
				let expression = self.expression()?;
				match &expression {
					Expression::Branch { .. } => {}
					Expression::Variable { .. } => {
						if self.accept(TokenVariant::Equal)?.is_some() {
							let name = expression;
							let value = self.expression()?;
							return Ok(Statement::Assignment { name, value });
						}
						self.expect(TokenVariant::SemiColon)?;
					}
					_ => {
						self.expect(TokenVariant::SemiColon)?;
					}
				}
				Ok(Statement::Expression(expression))
			}
		}
	}

	fn struct_declaration(&mut self) -> Result<Statement, ParseError> {
		self.expect(TokenVariant::Struct)?;
		let struct_name = self.expect(TokenVariant::Identifier)?;
		self.expect(TokenVariant::LeftCurly)?;

		let mut members = Vec::with_capacity(64);

		while let Some(member_name) = self.accept(TokenVariant::Identifier)? {
			self.expect(TokenVariant::Colon)?;
			let variant_name = self.expect(TokenVariant::Identifier)?;

			members.push(Declaration {
				name: member_name,
				variant: variant_name,
			});

			if self.accept(TokenVariant::Comma)?.is_none() {
				break;
			}
		}

		self.expect(TokenVariant::RightCurly)?;
		Ok(Statement::StructDeclaration {
			name: struct_name,
			members,
		})
	}

	fn function_declaration(&mut self) -> Result<Statement, ParseError> {
		self.expect(TokenVariant::Fn)?;
		let name = self.expect(TokenVariant::Identifier)?;
		self.expect(TokenVariant::LeftParen)?;

		let mut params = Vec::with_capacity(64);

		while let Some(name) = self.accept(TokenVariant::Identifier)? {
			self.expect(TokenVariant::Colon)?;
			let variant = self.expect(TokenVariant::Identifier)?;

			params.push(Declaration { name, variant });

			if self.accept(TokenVariant::Comma)?.is_none() {
				break;
			}
		}

		self.expect(TokenVariant::RightParen)?;

		let result = if self.accept(TokenVariant::Arrow)?.is_some() {
			Some(self.expect(TokenVariant::Identifier)?)
		} else {
			None
		};

		let body = self.body()?;

		Ok(Statement::FuntionDeclaration {
			name,
			params,
			result,
			body,
		})
	}

	fn expression(&mut self) -> Result<Expression, ParseError> {
		match self.peek()?.variant {
			TokenVariant::Number
			| TokenVariant::Char
			| TokenVariant::String
			| TokenVariant::Bool => Ok(Expression::Constant {
				token: self.next()?,
			}),
			TokenVariant::Identifier => {
				let name = self.next()?;
				match self.peek()?.variant {
					// Struct literals
					TokenVariant::LeftCurly => {
						self.expect(TokenVariant::LeftCurly)?;

						let mut members = Vec::with_capacity(1024);
						while let Some(name) = self.accept(TokenVariant::Identifier)? {
							self.expect(TokenVariant::Colon)?;
							let expression = self.expression()?;

							members.push(StructLiteralDefinition { name, expression });

							if self.accept(TokenVariant::Comma)?.is_none() {
								break;
							}
						}

						self.expect(TokenVariant::RightCurly)?;

						Ok(Expression::StructLiteral {
							variant: name,
							members,
						})
					}
					TokenVariant::LeftParen => {
						self.expect(TokenVariant::LeftParen)?;
						if self.accept(TokenVariant::RightParen)?.is_some() {
							return Ok(Expression::FunctionCall {
								name,
								params: Vec::default(),
							});
						}

						let mut params = Vec::with_capacity(64);
						loop {
							params.push(self.expression()?);

							if self.accept(TokenVariant::Comma)?.is_none() {
								break;
							}
						}

						self.expect(TokenVariant::RightParen)?;

						Ok(Expression::FunctionCall { name, params })
					}
					TokenVariant::Period => {
						let mut accesses = Vec::with_capacity(64);

						loop {
							if self.accept(TokenVariant::Period)?.is_some() {
								accesses.push(self.expect(TokenVariant::Identifier)?);
							} else {
								break;
							}
						}
						Ok(Expression::Variable { name, accesses })
					}
					_ => Ok(Expression::Variable {
						name,
						accesses: Vec::default(),
					}),
				}
			}
			TokenVariant::If => {
				self.expect(TokenVariant::If)?;

				let if_branch = Branch {
					condition: self.expression()?,
					body: self.body()?,
				};

				let mut else_if_branches = Vec::with_capacity(64);
				let mut else_body = None;

				loop {
					if self.accept(TokenVariant::Else)?.is_some() {
						if self.accept(TokenVariant::If)?.is_some() {
							else_if_branches.push(Branch {
								condition: self.expression()?,
								body: self.body()?,
							});
						} else {
							else_body = Some(self.body()?);
						}
					} else {
						break;
					}
				}

				Ok(Expression::Branch {
					if_branch: Box::new(if_branch),
					else_if_branches,
					else_body,
				})
			}
			_ => Err(ParseError::unexpected_token(
				&[
					TokenVariant::Number,
					TokenVariant::Char,
					TokenVariant::String,
					TokenVariant::Bool,
					TokenVariant::Identifier,
					TokenVariant::If,
				],
				self.next()?,
			)),
		}
	}
}
