use std::error;
use std::fmt;
use std::iter::Iterator;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenVariant {
	Identifier,

	// Keywords
	If,
	Else,
	Let,
	While,
	For,
	Loop,
	Fn,
	Struct,
	Mut,
	Return,

	LeftCurly,
	RightCurly,
	LeftBracket,
	RightBracket,
	LeftParen,
	RightParen,
	Colon,
	SemiColon,
	Comma,
	Period,
	Equal,
	NotEqual,
	LessThan,
	GreaterThan,
	LessThanEqual,
	GreaterThanEqual,
	Bang,
	Backslash,
	Plus,
	Minus,
	Asterisk,
	Arrow,

	// Literals
	Char,
	String,
	Number,
	Bool,

	LineComment,
	BlockComment,

	EndOfFile,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub variant: TokenVariant,

	pub contents: String,
	pub line: usize,
	pub column: usize,
}

#[derive(Debug)]
pub enum LexerError {
	UnterminatedSingleQuote,
	UnterminatedDoubleQuote,
	InvalidCharacterLiteral,
	UnterminatedBlockComment,
	MultipleDecimalPoints,
	InvalidNumberLiteral,
}

impl fmt::Display for LexerError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl error::Error for LexerError {}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
	contents: &'a str,

	peek: Option<Token>,

	line: usize,
	column: usize,

	ignore_comments: bool,
}

impl<'a> Lexer<'a> {
	pub fn new(contents: &'a str) -> Self {
		Self {
			contents,

			peek: None,

			line: 1,
			column: 1,

			ignore_comments: false,
		}
	}

	pub fn ignore_comments(mut self) -> Self {
		self.ignore_comments = true;
		self
	}

	fn advance(&mut self, amount: usize) -> &'a str {
		let len = self.contents.len();
		assert!(amount <= len);

		for (index, c) in self.contents.char_indices() {
			assert!(index <= amount);
			if index == amount {
				break;
			} else {
				match c {
					'\n' => {
						self.line += 1;
						self.column = 1
					}
					'\r' => self.column = 1,
					_ => self.column += 1,
				}
			}
		}

		let result = &self.contents[0..amount];
		self.contents = &self.contents[amount..len];
		result
	}

	pub fn next(&mut self) -> Result<Token, LexerError> {
		let line = self.line;
		let column = self.column;

		let c = match self.contents.chars().next() {
			Some(c) => c,
			None => {
				return Ok(Token {
					variant: TokenVariant::EndOfFile,

					contents: self.contents.to_string(),
					line,
					column,
				})
			}
		};

		if c.is_whitespace() {
			self.advance(c.len_utf8());
			return self.next();
		}

		// If we're not ignoring comments then we can pop the peek
		if !self.ignore_comments {
			if let Some(peek) = self.peek.take() {
				self.advance(peek.contents.len());
				return Ok(peek);
			}
		}

		if c == '/' {
			if let Some(next) = self.contents.chars().nth(1) {
				if next == '/' {
					let contents = if let Some((advance, _)) =
						self.contents.char_indices().find(|(_, c)| *c == '\n')
					{
						self.advance(advance + 1)
					} else {
						self.contents
					}
					.to_string();

					if self.ignore_comments {
						return self.next();
					} else {
						return Ok(Token {
							variant: TokenVariant::LineComment,

							contents,
							line,
							column,
						});
					}
				} else if next == '*' {
					let mut level = 0;
					let mut chars = self.contents.char_indices().peekable();
					while let Some((_, c)) = chars.next() {
						match c {
							'/' => {
								if let Some((_, next)) = chars.peek() {
									if *next == '*' {
										level += 1;
										chars.next();
									}
								}
							}
							'*' => {
								if let Some((_, next)) = chars.peek() {
									if *next == '/' {
										level -= 1;
										chars.next();
									}
								}
							}
							_ => {}
						}

						if level == 0 {
							break;
						}
					}

					if level > 0 {
						return Err(LexerError::UnterminatedBlockComment);
					}

					let len = if let Some((len, _)) = chars.next() {
						len
					} else {
						self.contents.len()
					};

					let contents = self.advance(len).to_string();
					if self.ignore_comments {
						return self.next();
					} else {
						return Ok(Token {
							variant: TokenVariant::BlockComment,

							contents,
							line,
							column,
						});
					}
				} else {
					let contents = self.advance(1).to_string();
					return Ok(Token {
						variant: TokenVariant::Backslash,

						contents,
						line,
						column,
					});
				}
			} else {
				let contents = self.advance(1).to_string();
				return Ok(Token {
					variant: TokenVariant::Backslash,

					contents,
					line,
					column,
				});
			}
		}

		// If we're ignoring the comments we need to wait until their eaten before
		// popping the peek
		if self.ignore_comments {
			if let Some(peek) = self.peek.take() {
				self.advance(peek.contents.len());
				return Ok(peek);
			}
		}

		// Identifier or Keyword
		// Identifiers can start with a letter, '_', but not a number
		if c.is_alphabetic() || c == '_' {
			let len = self
				.contents
				.char_indices()
				.find(|(_, c)| !(c.is_alphanumeric() || *c == '_'))
				.map(|(index, _)| index)
				.unwrap_or(self.contents.len());

			let identifier = self.advance(len);

			// Keyword matching
			let variant = match identifier {
				"if" => TokenVariant::If,
				"else" => TokenVariant::Else,
				"let" => TokenVariant::Let,
				"while" => TokenVariant::While,
				"for" => TokenVariant::For,
				"loop" => TokenVariant::Loop,
				"fn" => TokenVariant::Fn,
				"struct" => TokenVariant::Struct,
				"mut" => TokenVariant::Mut,
				"return" => TokenVariant::Return,
				"true" => TokenVariant::Bool,
				"false" => TokenVariant::Bool,
				_ => TokenVariant::Identifier,
			};

			return Ok(Token {
				variant,

				contents: identifier.to_string(),
				line,
				column,
			});
		}

		// Number literal tokenization
		if c.is_numeric() {
			if self.contents.starts_with("0x") {
				todo!("Hex")
			} else {
				let mut len = 0;
				let mut decimal_point = false;
				for (i, c) in self.contents.chars().enumerate() {
					if c == '.' {
						if decimal_point {
							return Err(LexerError::MultipleDecimalPoints);
						}
						decimal_point = true;
						continue;
					}

					if c.is_alphabetic() || (i == 0 && (c != '-' && !c.is_numeric())) {
						return Err(LexerError::InvalidNumberLiteral);
					}

					if !c.is_numeric() {
						len = i;
						break;
					}
				}

				let number = self.advance(len);
				return Ok(Token {
					variant: TokenVariant::Number,

					contents: number.to_string(),
					line,
					column,
				});
			}
		} else if c == '\'' {
			// Character literal tokenization
			if let Some(value) = self.contents.chars().nth(1) {
				if value == '\'' {
					return Err(LexerError::InvalidCharacterLiteral);
				}

				if let Some(end) = self.contents.chars().nth(2) {
					if end == '\'' {
						let contents = self.advance(2 + value.len_utf8()).to_string();
						return Ok(Token {
							variant: TokenVariant::Char,

							contents,
							line,
							column,
						});
					} else {
						return Err(LexerError::UnterminatedSingleQuote);
					}
				} else {
					return Err(LexerError::UnterminatedSingleQuote);
				}
			} else {
				return Err(LexerError::UnterminatedSingleQuote);
			}
		} else if c == '\"' {
			// String literal tokenization
			let mut chars = self.contents.char_indices();
			chars.next(); // Advance past "

			if let Some((len, _)) = chars.find(|(_, c)| *c == '"') {
				let string = self.advance(len + 1).to_string();
				return Ok(Token {
					variant: TokenVariant::String,

					contents: string,
					line,
					column,
				});
			} else {
				return Err(LexerError::UnterminatedDoubleQuote);
			}
		}

		// Arrow operator tokenization
		if c == '-' {
			let token = if let Some(next) = self.contents.chars().nth(1) {
				if next == '>' {
					let contents = self.advance("->".len()).to_string();
					Token {
						variant: TokenVariant::Arrow,

						contents,
						line,
						column,
					}
				} else {
					let contents = self.advance(1).to_string();
					Token {
						variant: TokenVariant::Minus,

						contents,
						line,
						column,
					}
				}
			} else {
				let contents = self.advance(1).to_string();
				Token {
					variant: TokenVariant::Minus,

					contents,
					line,
					column,
				}
			};
			Ok(token)
		} else if c == '<' {
			let token = if let Some(next) = self.contents.chars().nth(1) {
				if next == '=' {
					let contents = self.advance("<=".len()).to_string();
					Token {
						variant: TokenVariant::LessThanEqual,

						contents,
						line,
						column,
					}
				} else {
					let contents = self.advance(1).to_string();
					Token {
						variant: TokenVariant::LessThan,

						contents,
						line,
						column,
					}
				}
			} else {
				let contents = self.advance(1).to_string();
				Token {
					variant: TokenVariant::LessThan,

					contents,
					line,
					column,
				}
			};
			Ok(token)
		} else if c == '>' {
			let token = if let Some(next) = self.contents.chars().nth(1) {
				if next == '=' {
					let contents = self.advance(">=".len()).to_string();
					Token {
						variant: TokenVariant::GreaterThanEqual,

						contents,
						line,
						column,
					}
				} else {
					let contents = self.advance(1).to_string();
					Token {
						variant: TokenVariant::GreaterThan,

						contents,
						line,
						column,
					}
				}
			} else {
				let contents = self.advance(1).to_string();
				Token {
					variant: TokenVariant::GreaterThan,

					contents,
					line,
					column,
				}
			};
			Ok(token)
		} else if c == '!' {
			let token = if let Some(next) = self.contents.chars().nth(1) {
				if next == '=' {
					let contents = self.advance("!=".len()).to_string();
					Token {
						variant: TokenVariant::NotEqual,

						contents,
						line,
						column,
					}
				} else {
					let contents = self.advance(1).to_string();
					Token {
						variant: TokenVariant::NotEqual,

						contents,
						line,
						column,
					}
				}
			} else {
				let contents = self.advance(1).to_string();
				Token {
					variant: TokenVariant::NotEqual,

					contents,
					line,
					column,
				}
			};
			Ok(token)
		} else {
			let variant = match c {
				'{' => TokenVariant::LeftCurly,
				'}' => TokenVariant::RightCurly,
				'[' => TokenVariant::LeftBracket,
				']' => TokenVariant::RightBracket,
				'(' => TokenVariant::LeftParen,
				')' => TokenVariant::RightParen,
				':' => TokenVariant::Colon,
				';' => TokenVariant::SemiColon,
				',' => TokenVariant::Comma,
				'.' => TokenVariant::Period,
				'=' => TokenVariant::Equal,
				'/' => TokenVariant::Backslash,
				'+' => TokenVariant::Plus,
				'*' => TokenVariant::Asterisk,
				_ => unreachable!(),
			};
			let contents = self.advance(c.len_utf8()).to_string();
			Ok(Token {
				variant,

				contents,
				line,
				column,
			})
		}
	}

	pub fn peek(&mut self) -> Result<Token, LexerError> {
		if let Some(peek) = self.peek.as_ref() {
			Ok(peek.clone())
		} else {
			let mut clone = self.clone();
			self.peek = Some(clone.next()?);
			Ok(self.peek.as_ref().unwrap().clone())
		}
	}
}
