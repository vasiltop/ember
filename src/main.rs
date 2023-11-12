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

    println!("{:#?}", lexer::parse(&bytes));
}
