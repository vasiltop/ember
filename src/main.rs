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

    let instrctions = instruction::parse(tokens);
    //println!("{:#?}", instrctions);
}
