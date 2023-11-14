use crate::body::Body;
use crate::lexer::{Arithmetic, Delimeter, Identifier, Keyword, Literal, Op, Token};
use crate::scope::Scope;
use std::collections::VecDeque;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum InstructionError {
    #[error("Function did not return a value")]
    InvalidFunctionReturn,
    #[error("Invalid variable name")]
    InvalidVariableName,
    #[error("Could not resolve expression")]
    InvalidExpression,
    #[error("Invalid instruction")]
    InvalidInstruction,
    #[error("Invalid function call")]
    InvalidFunctionCall,
    #[error("Missing token: {token:?}")]
    MissingToken { token: Token },
    #[error("Invalid token")]
    InvalidToken { token: Token },
}

#[derive(Debug)]
pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn next(&mut self) -> Option<Token> {
        let token = self.tokens.pop_front();

        while self.tokens.front() == Some(&Token::WhiteSpace) {
            self.tokens.pop_front();
        }
        token
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    For {
        setup: Box<Instruction>,
        condition: Expression,
        post: Box<Instruction>,
        body: Body,
    },
    If {
        condition: Expression,
        body: Body,
        else_body: Body,
    },
    Let {
        ident: Identifier,
        expression: Expression,
    },
    Print {
        expression: Expression,
    },
    Fn {
        ident: Identifier,
        args: Vec<Identifier>,
        body: Body,
    },
    While {
        condition: Expression,
        body: Body,
    },
    Reassign {
        ident: Identifier,
        expression: Expression,
    },
    FnCall {
        ident: Identifier,
        args: Vec<Expression>,
    },
    Return {
        expression: Expression,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Operation {
        lhs: Box<Expression>,
        op: ExpressionArithmetic,
        rhs: Box<Expression>,
    },
    Literal(Literal),
    Identifier(Identifier),
    Function {
        ident: Identifier,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum ExpressionArithmetic {
    Arithmetic(Arithmetic),
    Op(Op),
}

impl Expression {
    pub fn resolve(&self, scope: Scope) -> Result<(Scope, Literal), InstructionError> {
        match self {
            Expression::Function { ident, args } => {
                let func = scope.get_func(ident).unwrap().clone();
                let (s, value) = func.resolve(scope, args)?;

                match value {
                    Some(value) => Ok((s, value)),
                    None => Err(InstructionError::InvalidFunctionReturn),
                }
            }
            Expression::Literal(literal) => Ok((scope, literal.clone())),
            Expression::Identifier(ident) => {
                if let Some(literal) = scope.get(ident).cloned() {
                    Ok((scope, literal))
                } else {
                    Err(InstructionError::InvalidVariableName)
                }
            }

            Expression::Operation { lhs, op, rhs } => {
                let (s, lhs) = lhs.resolve(scope)?;
                let (s, rhs) = rhs.resolve(s)?;

                Ok((
                    s,
                    match (op, lhs, rhs) {
                        (_, Literal::Number(lhs), Literal::Number(rhs)) => match op {
                            ExpressionArithmetic::Arithmetic(Arithmetic::Plus) => {
                                Literal::Number(lhs + rhs)
                            }
                            ExpressionArithmetic::Arithmetic(Arithmetic::Minus) => {
                                Literal::Number(lhs - rhs)
                            }
                            ExpressionArithmetic::Arithmetic(Arithmetic::Mul) => {
                                Literal::Number(lhs * rhs)
                            }
                            ExpressionArithmetic::Arithmetic(Arithmetic::Div) => {
                                Literal::Number(lhs / rhs)
                            }
                            ExpressionArithmetic::Arithmetic(Arithmetic::Mod) => {
                                Literal::Number(lhs % rhs)
                            }
                            ExpressionArithmetic::Op(Op::Eq) => Literal::Boolean(lhs == rhs),
                            ExpressionArithmetic::Op(Op::Ge) => Literal::Boolean(lhs >= rhs),
                            ExpressionArithmetic::Op(Op::Gt) => Literal::Boolean(lhs > rhs),
                            ExpressionArithmetic::Op(Op::Le) => Literal::Boolean(lhs <= rhs),
                            ExpressionArithmetic::Op(Op::Lt) => Literal::Boolean(lhs < rhs),
                            ExpressionArithmetic::Op(Op::Ne) => Literal::Boolean(lhs != rhs),
                            _ => Err(InstructionError::InvalidExpression)?,
                        },
                        (
                            ExpressionArithmetic::Arithmetic(Arithmetic::Plus),
                            Literal::String(lhs),
                            Literal::String(rhs),
                        ) => Literal::String(lhs + &rhs),
                        (_, Literal::Boolean(lhs), Literal::Boolean(rhs)) => match op {
                            ExpressionArithmetic::Op(Op::And) => Literal::Boolean(lhs && rhs),
                            ExpressionArithmetic::Op(Op::Or) => Literal::Boolean(lhs || rhs),
                            _ => Err(InstructionError::InvalidExpression)?,
                        },
                        _ => Err(InstructionError::InvalidExpression)?,
                    },
                ))
            }
        }
    }

    fn parse(tokens: &mut Parser) -> Result<Self, InstructionError> {
        let mut lhs = Self::parse_comparison(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Op(op @ (Op::And | Op::Or)) => {
                    let op = *op;
                    tokens.next();
                    let rhs = Self::parse_comparison(tokens)?;
                    lhs = Expression::Operation {
                        lhs: Box::new(lhs),
                        op: ExpressionArithmetic::Op(op),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn parse_comparison(tokens: &mut Parser) -> Result<Self, InstructionError> {
        let mut lhs = Self::parse_plus_minus(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Op(op @ (Op::Eq | Op::Ge | Op::Gt | Op::Le | Op::Lt | Op::Ne)) => {
                    let op = *op;
                    tokens.next();
                    let rhs = Self::parse_plus_minus(tokens)?;
                    lhs = Expression::Operation {
                        lhs: Box::new(lhs),
                        op: ExpressionArithmetic::Op(op),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn parse_plus_minus(tokens: &mut Parser) -> Result<Self, InstructionError> {
        let mut lhs = Self::parse_mul_div_mod(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Arithmetic(op @ (Arithmetic::Plus | Arithmetic::Minus)) => {
                    let op = *op;
                    tokens.next();
                    let rhs = Self::parse_mul_div_mod(tokens)?;
                    lhs = Expression::Operation {
                        lhs: Box::new(lhs),
                        op: ExpressionArithmetic::Arithmetic(op),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn parse_mul_div_mod(tokens: &mut Parser) -> Result<Self, InstructionError> {
        let mut lhs = Self::parse_paren_literal_ident(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Arithmetic(op @ (Arithmetic::Mul | Arithmetic::Div | Arithmetic::Mod)) => {
                    let op = *op;
                    tokens.next();

                    let rhs = Self::parse_paren_literal_ident(tokens)?;
                    lhs = Expression::Operation {
                        lhs: Box::new(lhs),
                        op: ExpressionArithmetic::Arithmetic(op),
                        rhs: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn parse_paren_literal_ident(tokens: &mut Parser) -> Result<Self, InstructionError> {
        match tokens.next() {
            Some(Token::Identifier(ident)) => match tokens.peek() {
                Some(Token::Delimeter(Delimeter::ParenOpen)) => {
                    tokens.next();
                    let mut args = Vec::new();

                    if tokens.peek() != Some(&Token::Delimeter(Delimeter::ParenClose)) {
                        args.push(Expression::parse(tokens)?);
                    }

                    while let Some(token) = tokens.peek() {
                        match token {
                            Token::Delimeter(Delimeter::ParenClose) => break,
                            Token::Comma => {
                                tokens.next();
                                args.push(Expression::parse(tokens)?);
                            }
                            _ => {
                                return Err(InstructionError::InvalidFunctionCall);
                            }
                        }
                    }

                    match tokens.next() {
                        Some(Token::Delimeter(Delimeter::ParenClose)) => {}
                        _ => return Err(InstructionError::InvalidFunctionCall),
                    }

                    Ok(Expression::Function { ident, args })
                }
                _ => Ok(Expression::Identifier(ident)),
            },

            Some(Token::Literal(literal)) => Ok(Expression::Literal(literal)),

            Some(Token::Delimeter(Delimeter::ParenOpen)) => {
                let expr = Expression::parse(tokens)?;
                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::ParenClose)) => Ok(expr),
                    _ => Err(InstructionError::InvalidExpression),
                }
            }
            _ => Err(InstructionError::InvalidExpression),
        }
    }
}

pub fn parse(tokens: VecDeque<Token>) -> Result<Vec<Instruction>, InstructionError> {
    let mut tokens = Parser { tokens };
    let mut instructions = Vec::new();
    while let Some(Token::WhiteSpace) = tokens.peek() {
        tokens.next();
    }

    while tokens.peek().is_some() {
        instructions.push(Instruction::parse(&mut tokens)?);
    }

    Ok(instructions)
}

impl Instruction {
    pub fn parse(tokens: &mut Parser) -> Result<Instruction, InstructionError> {
        match tokens.next() {
            Some(Token::Keyword(Keyword::Return)) => {
                let expression = Expression::parse(tokens)?;

                match tokens.next() {
                    Some(Token::Semicolon) => Ok(Instruction::Return { expression }),
                    _ => Err(InstructionError::MissingToken {
                        token: Token::Semicolon,
                    }),
                }
            }
            Some(Token::Keyword(Keyword::Let)) => {
                let ident = match tokens.next() {
                    Some(Token::Identifier(ident)) => ident,
                    token => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Identifier(Identifier("Variable Name".to_string())),
                        })
                    }
                };

                match tokens.next() {
                    Some(Token::Eq) => {}
                    _ => return Err(InstructionError::MissingToken { token: Token::Eq }),
                }

                let expression = Expression::parse(tokens)?;

                match tokens.next() {
                    Some(Token::Semicolon) => Ok(Instruction::Let { ident, expression }),
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Semicolon,
                        })
                    }
                }
            }
            Some(Token::Keyword(Keyword::If)) => {
                let condition = Expression::parse(tokens)?;
                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Delimeter(Delimeter::CurlyOpen),
                        })
                    }
                }

                let body = Body::parse(tokens)?;

                match tokens.peek() {
                    Some(Token::Keyword(Keyword::Else)) => {
                        tokens.next();
                        match tokens.next() {
                            Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                            _ => {
                                return Err(InstructionError::MissingToken {
                                    token: Token::Delimeter(Delimeter::CurlyOpen),
                                })
                            }
                        }
                        let else_body = Body::parse(tokens)?;

                        Ok(Instruction::If {
                            condition,
                            body,
                            else_body,
                        })
                    }
                    _ => Ok(Instruction::If {
                        condition,
                        body,
                        else_body: Body {
                            instructions: Vec::new(),
                        },
                    }),
                }
            }
            Some(Token::Keyword(Keyword::For)) => {
                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::ParenOpen)) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Delimeter(Delimeter::ParenOpen),
                        })
                    }
                }

                let setup = Box::new(Instruction::parse(tokens)?);

                let condition = Expression::parse(tokens)?;

                match tokens.next() {
                    Some(Token::Semicolon) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Semicolon,
                        })
                    }
                }

                let post = Box::new(Instruction::parse(tokens)?);

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::ParenClose)) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Delimeter(Delimeter::ParenClose),
                        })
                    }
                }

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Delimeter(Delimeter::CurlyOpen),
                        })
                    }
                }

                let body = Body::parse(tokens)?;

                Ok(Instruction::For {
                    setup,
                    condition,
                    post,
                    body,
                })
            }
            Some(Token::Keyword(Keyword::Print)) => {
                let expression = Expression::parse(tokens)?;
                match tokens.next() {
                    Some(Token::Semicolon) => Ok(Instruction::Print { expression }),
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Semicolon,
                        })
                    }
                }
            }
            Some(Token::Keyword(Keyword::Fn)) => {
                let ident = match tokens.next() {
                    Some(Token::Identifier(ident)) => ident,
                    token => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Identifier(Identifier("Function Name".to_string())),
                        })
                    }
                };

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::ParenOpen)) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Delimeter(Delimeter::ParenOpen),
                        })
                    }
                }

                let mut args = Vec::new();

                while let Some(Token::Identifier(ident)) = tokens.next() {
                    args.push(ident);
                    match tokens.next() {
                        Some(Token::Delimeter(Delimeter::ParenClose)) => break,
                        Some(Token::Comma) => {}
                        _ => {
                            return Err(InstructionError::MissingToken {
                                token: Token::Delimeter(Delimeter::ParenClose),
                            })
                        }
                    }
                }

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Delimeter(Delimeter::CurlyOpen),
                        })
                    }
                }

                let body = Body::parse(tokens)?;

                Ok(Instruction::Fn { ident, args, body })
            }
            Some(Token::Keyword(Keyword::While)) => {
                let condition = Expression::parse(tokens)?;
                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => {
                        return Err(InstructionError::MissingToken {
                            token: Token::Delimeter(Delimeter::CurlyOpen),
                        })
                    }
                }

                let body = Body::parse(tokens)?;

                Ok(Instruction::While { condition, body })
            }
            Some(Token::Identifier(ident)) => match tokens.next() {
                Some(Token::Eq) => {
                    let expression = Expression::parse(tokens)?;

                    match tokens.next() {
                        Some(Token::Semicolon) => Ok(Instruction::Reassign { ident, expression }),
                        _ => Err(InstructionError::MissingToken {
                            token: Token::Semicolon,
                        }),
                    }
                }
                Some(Token::Delimeter(Delimeter::ParenOpen)) => {
                    let mut args = Vec::new();

                    while let Some(token) = tokens.peek() {
                        match token {
                            Token::Delimeter(Delimeter::ParenClose) => break,
                            Token::Comma => {
                                tokens.next();
                            }
                            _ => {
                                args.push(Expression::parse(tokens)?);
                            }
                        }
                    }

                    match tokens.next() {
                        Some(Token::Delimeter(Delimeter::ParenClose)) => {}
                        _ => {
                            return Err(InstructionError::MissingToken {
                                token: Token::Delimeter(Delimeter::ParenClose),
                            })
                        }
                    }

                    match tokens.next() {
                        Some(Token::Semicolon) => Ok(Instruction::FnCall { ident, args }),
                        _ => {
                            return Err(InstructionError::MissingToken {
                                token: Token::Semicolon,
                            })
                        }
                    }
                }
                _ => return Err(InstructionError::MissingToken { token: Token::Eq }),
            },
            token => Err(InstructionError::InvalidInstruction),
        }
    }
}
