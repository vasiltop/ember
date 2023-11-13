mod body;
mod instruction;
mod lexer;
use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
struct Args {
    path: PathBuf,
}

fn main() {
    let args = Args::parse();

    let bytes = fs::read(args.path).unwrap();
    let tokens = lexer::parse(&bytes).unwrap();
    println!("{:#?}", tokens);
    let instruction = instruction::parse(tokens);
    println!("{:#?}", instruction);
}
