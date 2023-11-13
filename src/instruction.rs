use crate::body::Body;
use crate::lexer::{Arithmetic, Delimeter, Identifier, Keyword, Literal, Op, Token};
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
        //println!("{:?}", token);
        token
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }
}

#[derive(Debug)]
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
        return_type: Option<Expression>,
    },
    While {
        condition: Expression,
        body: Body,
    },
}

#[derive(Debug)]
enum Expression {
    Operation {
        lhs: Box<Expression>,
        op: ExpressionArithmetic,
        rhs: Box<Expression>,
    },
    Literal(Literal),
    Identifier(Identifier),
}

#[derive(Debug)]
enum ExpressionArithmetic {
    Arithmetic(Arithmetic),
    Op(Op),
}

impl Expression {
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
            Some(Token::Identifier(ident)) => Ok(Expression::Identifier(ident)),
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
        //println!("{:?}", tokens.peek());
        match tokens.next() {
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

                //println!("peeked {:?}", tokens.peek());
                match tokens.next() {
                    Some(Token::Keyword(Keyword::Else)) => {
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
            //Some(Token::Keyword(Keyword::For)) => {}
            //Some(Token::Keyword(Keyword::Print)) => {}
            //Some(Token::Keyword(Keyword::Fn)) => {}
            // Some(Token::Keyword(Keyword::While)) => {}
            token => panic!("bad instruction: {:?}", token),
        }
    }
}
