use crate::body::Body;
use crate::lexer::{Arithmetic, Delimeter, Identifier, Keyword, Literal, Op, Token};
use crate::scope::Scope;
use std::collections::VecDeque;

#[derive(Debug)]
pub enum Error {
    ParserError,
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
    pub fn resolve(&self, scope: Scope) -> (Scope, Literal) {
        match self {
            Expression::Function { ident, args } => {
                let func = scope.get_func(ident).unwrap().clone();
                let (s, value) = func.resolve(scope, args);

                match value {
                    Some(value) => (s, value),
                    None => panic!("function did not return a value"),
                }
            }
            Expression::Literal(literal) => (scope, literal.clone()),
            Expression::Identifier(ident) => {
                if let Some(literal) = scope.get(ident).cloned() {
                    (scope, literal)
                } else {
                    panic!("could not find variable: {:?}", ident)
                }
            }

            Expression::Operation { lhs, op, rhs } => {
                let (s, lhs) = lhs.resolve(scope);
                let (s, rhs) = rhs.resolve(s);

                (
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
                            _ => panic!("cannot resolve expression: {:?}", self),
                        },
                        (
                            ExpressionArithmetic::Arithmetic(Arithmetic::Plus),
                            Literal::String(lhs),
                            Literal::String(rhs),
                        ) => Literal::String(lhs + &rhs),
                        (_, Literal::Boolean(lhs), Literal::Boolean(rhs)) => match op {
                            ExpressionArithmetic::Op(Op::And) => Literal::Boolean(lhs && rhs),
                            ExpressionArithmetic::Op(Op::Or) => Literal::Boolean(lhs || rhs),
                            _ => panic!("cannot resolve expression: {:?}", self),
                        },
                        _ => panic!("cannot resolve expression: {:?}", self),
                    },
                )
            }
        }
    }

    fn parse(tokens: &mut Parser) -> Result<Self, Error> {
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

    fn parse_comparison(tokens: &mut Parser) -> Result<Self, Error> {
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

    fn parse_plus_minus(tokens: &mut Parser) -> Result<Self, Error> {
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

    fn parse_mul_div_mod(tokens: &mut Parser) -> Result<Self, Error> {
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

    fn parse_paren_literal_ident(tokens: &mut Parser) -> Result<Self, Error> {
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
                                panic!("could not find comma or paren close for the function")
                            }
                        }
                    }

                    match tokens.next() {
                        Some(Token::Delimeter(Delimeter::ParenClose)) => {}
                        _ => panic!("could not find paren close for the function call"),
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
                    _ => Err(Error::ParserError),
                }
            }
            _ => Err(Error::ParserError),
        }
    }
}

pub fn parse(tokens: VecDeque<Token>) -> Result<Vec<Instruction>, Error> {
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
    pub fn parse(tokens: &mut Parser) -> Result<Instruction, Error> {
        match tokens.next() {
            Some(Token::Keyword(Keyword::Return)) => {
                let expression = Expression::parse(tokens)?;

                match tokens.next() {
                    Some(Token::Semicolon) => Ok(Instruction::Return { expression }),
                    _ => panic!("could not find semicolon for the return statement"),
                }
            }
            Some(Token::Keyword(Keyword::Let)) => {
                let ident = match tokens.next() {
                    Some(Token::Identifier(ident)) => ident,
                    token => panic!("could not find the identifier for the variable: {token:?}"),
                };

                match tokens.next() {
                    Some(Token::Eq) => {}
                    _ => panic!("could not find equal sign for the variable"),
                }
                let expression = Expression::parse(tokens)?;

                match tokens.next() {
                    Some(Token::Semicolon) => Ok(Instruction::Let { ident, expression }),
                    _ => panic!("could not find semicolon for the variable"),
                }
            }
            Some(Token::Keyword(Keyword::If)) => {
                let condition = Expression::parse(tokens)?;
                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => panic!("could not find curly open for the if statement"),
                }

                let body = Body::parse(tokens)?;

                match tokens.peek() {
                    Some(Token::Keyword(Keyword::Else)) => {
                        tokens.next();
                        match tokens.next() {
                            Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                            _ => panic!("could not find curly open for the else statement"),
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
                    _ => panic!("could not find paren open for the for statement"),
                }

                let setup = Box::new(Instruction::parse(tokens)?);

                let condition = Expression::parse(tokens)?;

                match tokens.next() {
                    Some(Token::Semicolon) => {}
                    _ => panic!("could not find semicolon 2 for the for statement"),
                }

                let post = Box::new(Instruction::parse(tokens)?);

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::ParenClose)) => {}
                    _ => panic!("could not find paren close for the for statement"),
                }

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => panic!("could not find curly open for the for statement"),
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
                    _ => panic!("could not find semicolon for the print statement"),
                }
            }
            Some(Token::Keyword(Keyword::Fn)) => {
                let ident = match tokens.next() {
                    Some(Token::Identifier(ident)) => ident,
                    token => panic!("could not find the identifier for the function: {token:?}"),
                };

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::ParenOpen)) => {}
                    _ => panic!("could not find paren open for the function"),
                }

                let mut args = Vec::new();

                while let Some(Token::Identifier(ident)) = tokens.next() {
                    args.push(ident);
                    match tokens.next() {
                        Some(Token::Delimeter(Delimeter::ParenClose)) => break,
                        Some(Token::Comma) => {}
                        _ => panic!("could not find comma or paren close for the function"),
                    }
                }

                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => panic!("could not find curly open for the function"),
                }

                let body = Body::parse(tokens)?;

                Ok(Instruction::Fn { ident, args, body })
            }
            Some(Token::Keyword(Keyword::While)) => {
                let condition = Expression::parse(tokens)?;
                match tokens.next() {
                    Some(Token::Delimeter(Delimeter::CurlyOpen)) => {}
                    _ => panic!("could not find curly open for the while statement"),
                }

                let body = Body::parse(tokens)?;

                Ok(Instruction::While { condition, body })
            }
            Some(Token::Identifier(ident)) => match tokens.next() {
                Some(Token::Eq) => {
                    let expression = Expression::parse(tokens)?;

                    match tokens.next() {
                        Some(Token::Semicolon) => Ok(Instruction::Reassign { ident, expression }),
                        _ => panic!("could not find semicolon for the variable"),
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
                        _ => panic!("could not find paren close for the function call"),
                    }

                    match tokens.next() {
                        Some(Token::Semicolon) => Ok(Instruction::FnCall { ident, args }),
                        _ => panic!("could not find semicolon for the function call"),
                    }
                }
                _ => panic!("could not find equal sign for the variable"),
            },
            token => panic!("bad instruction: {:?}", token),
        }
    }
}
