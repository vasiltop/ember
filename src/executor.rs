use crate::{
    instruction::{Instruction, InstructionError},
    lexer::Literal,
    scope::Scope,
};

pub fn execute(
    instructions: &[Instruction],
    mut scope: Scope,
) -> Result<(Scope, Option<Literal>), InstructionError> {
    for instruction in instructions {
        match instruction {
            Instruction::Print { expression } => {
                let (s, value) = expression.resolve(scope)?;
                scope = s;
                println!("{}", value)
            }
            Instruction::Let { ident, expression } => {
                let (s, value) = expression.resolve(scope)?;
                scope = s;
                scope.set(ident.clone(), value);
            }
            Instruction::Reassign { ident, expression } => {
                let (s, value) = expression.resolve(scope)?;
                scope = s;
                scope.reassign(ident.clone(), value);
            }
            Instruction::If {
                condition,
                body,
                else_body,
            } => match condition.resolve(scope) {
                Ok((s, crate::lexer::Literal::Boolean(true))) => {
                    let mut child_scope = Scope::default();
                    child_scope.parent = Some(Box::new(s));
                    (child_scope, _) = execute(&body.instructions, child_scope)?;
                    scope = child_scope.close();
                }
                Ok((s, crate::lexer::Literal::Boolean(false))) => {
                    let mut child_scope = Scope::default();
                    child_scope.parent = Some(Box::new(s));
                    (child_scope, _) = execute(&else_body.instructions, child_scope)?;
                    scope = child_scope.close();
                }
                _ => panic!("Expected boolean literal"),
            },
            Instruction::While { condition, body } => loop {
                let (s, lit) = condition.resolve(scope)?;
                if let crate::lexer::Literal::Boolean(false) = lit {
                    scope = s;
                    break;
                }
                let mut child_scope = Scope::default();
                child_scope.parent = Some(Box::new(s));
                (child_scope, _) = execute(&body.instructions, child_scope)?;
                scope = child_scope.close();
            },
            Instruction::For {
                setup,
                condition,
                post,
                body,
            } => {
                let mut child_scope = Scope::default();
                child_scope.parent = Some(Box::new(scope));
                (child_scope, _) = execute(&[*setup.clone()], child_scope)?;

                loop {
                    let (s, lit) = condition.resolve(child_scope)?;
                    child_scope = s;
                    if let crate::lexer::Literal::Boolean(false) = lit {
                        break;
                    }

                    let mut inner_scope = Scope::default();
                    inner_scope.parent = Some(Box::new(child_scope));
                    (inner_scope, _) = execute(&body.instructions, inner_scope)?;
                    child_scope = inner_scope.close();
                    (child_scope, _) = execute(&[*post.clone()], child_scope)?;
                }

                scope = child_scope.close();
            }
            Instruction::Fn { ident, args, body } => {
                scope.set_func(
                    ident.clone(),
                    crate::scope::Fn {
                        ident: ident.clone(),
                        args: args.clone(),
                        body: body.clone(),
                    },
                );
            }
            Instruction::FnCall { ident, args } => {
                let func = scope.get_func(ident).unwrap().clone();
                let mut child_scope = Scope::default();

                for (arg, expression) in func.args.iter().zip(args.iter()) {
                    let (s, value) = expression.resolve(child_scope)?;
                    child_scope = s;
                    child_scope.set(arg.clone(), value);
                }

                child_scope.parent = Some(Box::new(scope));
                (child_scope, _) = execute(&func.body.instructions, child_scope)?;
                scope = child_scope.close();
            }
            Instruction::Return { expression } => {
                let (s, value) = expression.resolve(scope)?;
                return Ok((s, Some(value)));
            }
        }
    }
    Ok((scope, None))
}
