use crate::{
    body::Body,
    executor::execute,
    instruction::{Expression, InstructionError},
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

impl Fn {
    pub fn resolve(
        &self,
        scope: Scope,
        args: &[Expression],
    ) -> Result<(Scope, Option<Literal>), InstructionError> {
        let mut child_scope = Scope::default();
        child_scope.parent = Some(Box::new(scope));
        for (arg, expr) in self.args.iter().zip(args.iter()) {
            let (s, value) = expr.resolve(child_scope)?;
            child_scope = s;
            child_scope.set(arg.clone(), value);
        }

        let (s, value) = execute(&self.body.instructions, child_scope)?;

        Ok((s.close(), value))
    }
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
