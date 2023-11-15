mod body;
mod executor;
mod instruction;
mod lexer;
mod scope;

use clap::Parser;
use instruction::InstructionError;
use lexer::LexerError;
use std::fs;
use std::path::PathBuf;
use thiserror::Error;

#[derive(Parser)]
struct Args {
    path: PathBuf,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("lexer error: {0}")]
    LexerError(#[from] LexerError),
    #[error("parser error: {0}")]
    InstructionError(#[from] InstructionError),
}

fn main() -> Result<(), Error> {
    let args = Args::parse();

    let bytes = fs::read(args.path).unwrap();
    let tokens = lexer::parse(&bytes)?;
    //println!("{:#?}", tokens);
    let instructions = instruction::parse(tokens)?;
    //println!("{:#?}", instructions);

    executor::execute(
        &instructions,
        scope::Scope::default(),
        &mut std::io::stdout(),
    )?;

    Ok(())
}
