use crate::{
    body::Body,
    instruction::Expression,
    lexer::{Identifier, Literal},
};
use std::collections::HashMap;

#[derive(Default)]
pub struct Scope {
    pub variables: HashMap<Identifier, Literal>,
    pub functions: HashMap<Identifier, Fn>,
    pub parent: Option<Box<Scope>>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub ident: Identifier,
    pub args: Vec<Identifier>,
    pub body: Body,
}

impl Scope {
    pub fn set(&mut self, ident: Identifier, value: Literal) {
        self.variables.insert(ident, value);
    }

    pub fn reassign(&mut self, ident: Identifier, value: Literal) {
        if self.variables.contains_key(&ident) {
            self.variables.insert(ident, value);
        } else if let Some(parent) = &mut self.parent {
            parent.reassign(ident, value);
        }
    }

    pub fn get(&self, ident: &Identifier) -> Option<&Literal> {
        if let Some(value) = self.variables.get(ident) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get(ident)
        } else {
            None
        }
    }

    pub fn get_func(&self, ident: &Identifier) -> Option<&Fn> {
        if let Some(value) = self.functions.get(ident) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get_func(ident)
        } else {
            None
        }
    }

    pub fn set_func(&mut self, ident: Identifier, value: Fn) {
        self.functions.insert(ident, value);
    }

    pub fn close(self) -> Scope {
        *self.parent.unwrap()
    }
}
