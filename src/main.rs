mod lexer;

use clap::Parser;
use std::fs::File;
use std::{io::BufReader, path::PathBuf};

#[derive(Parser)]
struct Args {
    path: PathBuf,
}

fn main() {
    let args = Args::parse();

    let file = File::open(args.path).unwrap();
    let file = BufReader::new(file);
}
