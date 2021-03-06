#![feature(type_alias_impl_trait)]
use std::env;
use std::error;
use std::fmt::{
	self,
	Display,
};
use std::fs;
use std::io::Write;

use std::time::Instant;

mod lexer;
use lexer::*;

mod parser;
use parser::*;

mod analyzer;
use analyzer::*;

#[derive(Debug)]
enum ProgramError {
	NoArguments,
	InvalidPath,
	ParseError(ParseError),
}

impl Display for ProgramError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl error::Error for ProgramError {}

fn main() -> Result<(), ProgramError> {
	let args: Vec<String> = env::args().collect();

	if let Some(path) = args.get(1) {
		if path.ends_with(".toy") {
			if let Ok(file) = fs::read_to_string(path) {
				#[cfg(debug_assertions)]
				// Create a result file to write any extra debug data out to
				let output = fs::File::create(format!("{}.result", path)).unwrap();

				let now = Instant::now();
				let ast = { Parser::parse(&file).map_err(ProgramError::ParseError)? };
				let dur = Instant::now().duration_since(now);

				#[cfg(debug_assertions)]
				writeln!(&output, "{:#?}", ast).unwrap();

				println!("Parsed ({:?}) in {:?} ms", path, dur.as_secs_f64() * 1000.0);

				Ok(())
			} else {
				Err(ProgramError::InvalidPath)
			}
		} else {
			Err(ProgramError::InvalidPath)
		}
	} else {
		Err(ProgramError::NoArguments)
	}
}
