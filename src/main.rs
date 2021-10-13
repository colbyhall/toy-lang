#![feature(type_alias_impl_trait)]
use std::env;
use std::error;
use std::fmt::{ Display, self };
use std::fs;

mod tokenizer;
use tokenizer::*;

#[derive(Debug)]
enum ProgramError {
    NoArguments,
    InvalidPath
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
                println!("Compiling {:?}", path);

                for token in Tokenizer::new(&file) {
                    println!("{:?}", token);
                }
                
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
