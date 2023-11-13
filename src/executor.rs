use crate::{instruction::Instruction, scope::Scope};

pub fn execute(instructions: &[Instruction], mut scope: Scope) -> Scope {
    for instruction in instructions {
        match instruction {
            Instruction::Print { expression } => println!("{:?}", expression.resolve(&scope)),
            Instruction::Let { ident, expression } => {
                scope.set(ident.clone(), expression.resolve(&scope));
            }
            Instruction::Reassign { ident, expression } => {
                scope.reassign(ident.clone(), expression.resolve(&scope));
            }
            Instruction::If {
                condition,
                body,
                else_body,
            } => match condition.resolve(&scope) {
                crate::lexer::Literal::Boolean(true) => {
                    let mut child_scope = Scope::default();
                    child_scope.parent = Some(Box::new(scope));
                    child_scope = execute(&body.instructions, child_scope);
                    scope = child_scope.close();
                }
                crate::lexer::Literal::Boolean(false) => {
                    let mut child_scope = Scope::default();
                    child_scope.parent = Some(Box::new(scope));
                    child_scope = execute(&else_body.instructions, child_scope);
                    scope = child_scope.close();
                }
                _ => panic!("Expected boolean literal"),
            },
            Instruction::While { condition, body } => {
                while let crate::lexer::Literal::Boolean(true) = condition.resolve(&scope) {
                    let mut child_scope = Scope::default();
                    child_scope.parent = Some(Box::new(scope));
                    child_scope = execute(&body.instructions, child_scope);
                    scope = child_scope.close();
                }
            }
            Instruction::For {
                setup,
                condition,
                post,
                body,
            } => {
                let mut child_scope = Scope::default();
                child_scope.parent = Some(Box::new(scope));
                child_scope = execute(&[*setup.clone()], child_scope);

                while let crate::lexer::Literal::Boolean(true) = condition.resolve(&child_scope) {
                    let mut inner_scope = Scope::default();
                    inner_scope.parent = Some(Box::new(child_scope));
                    inner_scope = execute(&body.instructions, inner_scope);
                    child_scope = inner_scope.close();
                    child_scope = execute(&[*post.clone()], child_scope);
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

                for (arg, value) in func.args.iter().zip(args.iter()) {
                    child_scope.set(arg.clone(), value.resolve(&child_scope));
                }

                child_scope.parent = Some(Box::new(scope));
                child_scope = execute(&func.body.instructions, child_scope);
                scope = child_scope.close();
            }
            Instruction::Return { expression } => {}
        }
    }
    scope
}
