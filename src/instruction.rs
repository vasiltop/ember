use crate::lexer::{Arithmetic, Delimeter, Identifier, Keyword, Literal, Op, Token};
use std::collections::VecDeque;

#[derive(Debug)]
struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    fn next(&mut self) -> Option<Token> {
        let token = self.tokens.pop_front();

        while self.tokens.front() == Some(&Token::WhiteSpace) {
            self.tokens.pop_front();
        }

        token
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }
}

enum Instruction {
    For {},
    If {},
    Let {},
    Print {},
    Fn {},
}

#[derive(Debug)]
enum Expression {
    Operation {
        lhs: Box<Expression>,
        op: Arithmetic,
        rhs: Box<Expression>,
    },
    Literal(Literal),
    Identifier(Identifier),
}

#[derive(Debug)]
enum StackItem {
    Arithmetic(Arithmetic),
    Op(Op),
    ParenOpen,
}

impl Expression {
    fn parse(
        tokens: &mut Parser,
        stack: &mut Vec<StackItem>,
        queue: &mut VecDeque<Expression>,
    ) -> Expression {
        while let Some(token) = tokens.next() {
            match token {
                Token::Arithmetic(token @ (Arithmetic::Plus | Arithmetic::Minus)) => {
                    match stack.last() {
                        Some(StackItem::Arithmetic(
                            op @ (Arithmetic::Mul | Arithmetic::Div | Arithmetic::Mod),
                        )) => {
                            let rhs = Box::new(queue.pop_back().unwrap());
                            let lhs = Box::new(queue.pop_back().unwrap());
                            let op = *op;
                            stack.pop();
                            queue.push_back(Expression::Operation { rhs, lhs, op })
                        }
                        _ => {}
                    }
                    stack.push(StackItem::Arithmetic(token))
                }
                Token::Arithmetic(
                    token @ (Arithmetic::Mul | Arithmetic::Div | Arithmetic::Mod),
                ) => stack.push(StackItem::Arithmetic(token)),
                Token::Delimeter(Delimeter::ParenClose) => {
                    while let Some(op) = stack.pop() {
                        if let StackItem::Arithmetic(op) = op {
                            let rhs = Box::new(queue.pop_back().unwrap());
                            let lhs = Box::new(queue.pop_back().unwrap());
                            queue.push_back(Expression::Operation { rhs, lhs, op });
                        } else {
                            break;
                        }
                    }
                }
                Token::Delimeter(Delimeter::ParenOpen) => stack.push(StackItem::ParenOpen),
                Token::Identifier(ident) => queue.push_back(Expression::Identifier(ident)),
                Token::Literal(r) => queue.push_back(Expression::Literal(r)),
                Token::Semicolon => break,
                token => panic!("error while parsing expression {token:?}"),
            }
        }

        while let Some(op) = stack.pop() {
            if let StackItem::Arithmetic(op) = op {
                let rhs = Box::new(queue.pop_back().unwrap());
                let lhs = Box::new(queue.pop_back().unwrap());
                queue.push_back(Expression::Operation { rhs, lhs, op });
            } else {
                panic!("error while parsing expression");
            }
        }

        queue.pop_back().unwrap()
    }
}

pub fn parse(tokens: VecDeque<Token>) {
    let mut tokens = Parser { tokens };

    while let Some(Token::WhiteSpace) = tokens.peek() {
        tokens.next();
    }

    while let Some(token) = tokens.next() {
        match token {
            Token::Keyword(Keyword::Let) => {
                let ident = match tokens.next() {
                    Some(Token::Identifier(ident)) => ident,
                    token => panic!("could not find the identifier for the variable: {token:?}"),
                };

                match tokens.next() {
                    Some(Token::Eq) => {}
                    _ => panic!("could not find equal sign for the variable"),
                }

                let mut stack = Vec::new();
                let mut queue = VecDeque::new();
                let expression = Expression::parse(&mut tokens, &mut stack, &mut queue);
                println!("{:#?}", expression);
            }
            Token::Keyword(Keyword::If) => {}
            Token::Keyword(Keyword::For) => {}
            Token::Keyword(Keyword::Print) => {}
            Token::Keyword(Keyword::Fn) => {}
            Token::Keyword(Keyword::Else) => {}
            Token::Keyword(Keyword::While) => {}
            _ => {}
        }
    }
}
