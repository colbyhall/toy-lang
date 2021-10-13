use std::error;
use std::fmt;
use std::iter::Iterator;

#[derive(Debug, Copy, Clone)]
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

	LeftCurly,
	RightCurly,
	LeftBracket,
	RightBracket,
	OpenParen,
	CloseParen,
	Colon,
	SemiColon,
	Comma,
	Period,
	Equal,
	LessThan,
	GreaterThan,
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

	SingleLineComment,
	MultiLineComment,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
	pub variant: TokenVariant,

	pub contents: &'a str,
	pub line: usize,
	pub column: usize,
}

#[derive(Debug)]
pub enum LexerError {}

impl fmt::Display for LexerError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl error::Error for LexerError {}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
	contents: &'a str,

	line: usize,
	column: usize,
}

impl<'a> Lexer<'a> {
	pub fn new(contents: &'a str) -> Self {
		Self {
			contents,

			line: 1,
			column: 1,
		}
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

	pub fn peek(&self) -> Option<Token> {
		let mut clone = self.clone();
		clone.next()
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = Token<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		let c = self.contents.chars().next()?;

		let line = self.line;
		let column = self.column;

		if c.is_whitespace() {
			self.advance(c.len_utf8());
			return self.next();
		}
		// Identifier or Keyword
		// Identifiers can start with a letter, '_', but not a number
		else if c.is_alphabetic() || c == '_' {
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
				_ => TokenVariant::Identifier,
			};

			return Some(Token {
				variant,

				contents: identifier,
				line,
				column,
			});
		}

		if c.is_numeric() {
			// Number tokenization
			if self.contents.starts_with("0x") {
				todo!("Hex")
			} else {
				let mut len = 0;
				let mut decimal_point = false;
				for (i, c) in self.contents.chars().enumerate() {
					if c == '.' {
						if decimal_point {
							todo!("Error Handling");
						}
						decimal_point = true;
					}

					if !(c.is_numeric() || i == 0 && c == '-') {
						len = i;
						break;
					}
				}

				let number = self.advance(len);
				return Some(Token {
					variant: TokenVariant::Number,

					contents: number,
					line,
					column,
				});
			}
		} else if c == '\'' {
			// Character tokenization
			if let Some(value) = self.contents.chars().nth(1) {
				if let Some(end) = self.contents.chars().nth(2) {
					if end == '\'' {
						let contents = self.advance(2 + value.len_utf8());
						return Some(Token {
							variant: TokenVariant::Char,

							contents,
							line,
							column,
						});
					} else {
						todo!("Error Handling");
					}
				} else {
					todo!("Error Handling");
				}
			} else {
				todo!("Error Handling");
			}
		} else if c == '\"' {
			// String tokenization
			let mut chars = self.contents.char_indices();
			chars.next(); // Advance past "

			if let Some((len, _)) = chars.find(|(_, c)| *c == '"') {
				let string = self.advance(len + 1);
				return Some(Token {
					variant: TokenVariant::String,

					contents: string,
					line,
					column,
				});
			} else {
				todo!("Error Handling");
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
					};

					Some(Token {
						variant: TokenVariant::SingleLineComment,

						contents,
						line,
						column,
					})
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
						todo!("Error Handling");
					}

					let len = if let Some((len, _)) = chars.next() {
						len
					} else {
						self.contents.len()
					};

					let contents = self.advance(len);
					Some(Token {
						variant: TokenVariant::MultiLineComment,

						contents,
						line,
						column,
					})
				} else {
					let contents = self.advance(1);
					Some(Token {
						variant: TokenVariant::Backslash,

						contents,
						line,
						column,
					})
				}
			} else {
				let contents = self.advance(1);
				Some(Token {
					variant: TokenVariant::Backslash,

					contents,
					line,
					column,
				})
			}
		} else if c == '-' {
			let token = if let Some(next) = self.contents.chars().nth(1) {
				if next == '>' {
					let contents = self.advance("->".len());
					Token {
						variant: TokenVariant::Arrow,

						contents,
						line,
						column,
					}
				} else {
					let contents = self.advance(1);
					Token {
						variant: TokenVariant::Minus,

						contents,
						line,
						column,
					}
				}
			} else {
				let contents = self.advance(1);
				Token {
					variant: TokenVariant::Minus,

					contents,
					line,
					column,
				}
			};
			Some(token)
		} else {
			let variant = match c {
				'{' => TokenVariant::LeftCurly,
				'}' => TokenVariant::RightCurly,
				'[' => TokenVariant::LeftBracket,
				']' => TokenVariant::RightBracket,
				'(' => TokenVariant::OpenParen,
				')' => TokenVariant::CloseParen,
				':' => TokenVariant::Colon,
				';' => TokenVariant::SemiColon,
				',' => TokenVariant::Comma,
				'.' => TokenVariant::Period,
				'=' => TokenVariant::Equal,
				'<' => TokenVariant::LessThan,
				'>' => TokenVariant::GreaterThan,
				'!' => TokenVariant::Bang,
				'/' => TokenVariant::Backslash,
				'+' => TokenVariant::Plus,
				'*' => TokenVariant::Asterisk,
				_ => unreachable!(),
			};
			let contents = self.advance(c.len_utf8());
			Some(Token {
				variant,

				contents,
				line,
				column,
			})
		}
	}
}
