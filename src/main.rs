mod body;
mod executor;
mod instruction;
mod lexer;
mod scope;

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
    //println!("{:#?}", tokens);
    let instructions = instruction::parse(tokens).unwrap();
    //println!("{:#?}", instructions);
    executor::execute(&instructions, scope::Scope::default());
}
