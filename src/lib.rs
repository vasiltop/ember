mod body;
mod executor;
mod instruction;
mod lexer;
mod scope;

use instruction::InstructionError;
use lexer::LexerError;
use thiserror::Error;
use wasm_bindgen::prelude::wasm_bindgen;

#[derive(Debug, Error)]
pub enum Error {
    #[error("lexer error: {0}")]
    LexerError(#[from] LexerError),
    #[error("parser error: {0}")]
    InstructionError(#[from] InstructionError),
}

#[wasm_bindgen]
pub fn execute(bytes: &[u8]) -> Result<String, String> {
    let tokens = lexer::parse(bytes).map_err(|e| format!("{:?}", e))?;
    //println!("{:#?}", tokens);
    let instructions = instruction::parse(tokens).map_err(|e| format!("{:?}", e))?;
    //println!("{:#?}", instructions);
    let mut output = Vec::new();
    executor::execute(&instructions, scope::Scope::default(), &mut output)
        .map_err(|e| format!("{:?}", e))?;

    Ok(String::from_utf8_lossy(&output).into_owned())
}
