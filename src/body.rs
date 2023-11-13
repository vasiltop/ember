use crate::{
    instruction::{Error, Instruction, Parser},
    lexer::{Delimeter, Token},
};

#[derive(Debug)]
pub struct Body {
    pub instructions: Vec<Instruction>,
}

impl Body {
    pub fn parse(tokens: &mut Parser) -> Result<Self, Error> {
        let mut instructions = Vec::new();

        while let Some(token) = tokens.peek() {
            match token {
                Token::Delimeter(Delimeter::CurlyClose) => {
                    tokens.next();
                    break;
                }
                _ => {
                    instructions.push(Instruction::parse(tokens)?);
                }
            }
        }

        Ok(Self { instructions })
    }
}
