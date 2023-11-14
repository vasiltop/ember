use std::{collections::VecDeque, fmt::Display};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("invalid utf8")]
    InvalidUtf8,
    #[error("invalid number")]
    InvalidNumber,
    #[error("invalid token")]
    TokenError,
}

macro_rules! decode {
    ($bytes:expr, $name:ident) => {{
        if let Ok((r, Some(token))) = $name::decode($bytes) {
            return Ok((r, Some($crate::lexer::Token::$name(token))));
        }
    }};
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Arithmetic(Arithmetic),
    Op(Op),
    Literal(Literal),
    Identifier(Identifier),
    Delimeter(Delimeter),
    Eq,
    WhiteSpace,
    Eof,
    Semicolon,
    Comma,
}

trait RawTokenExt {
    fn is_whitespace(&self) -> bool;
    fn is_delim(&self) -> bool;
    fn is_punc(&self) -> bool;
}

impl RawTokenExt for u8 {
    fn is_whitespace(&self) -> bool {
        matches!(self, b' ' | b'\n' | b'\r' | b'\t')
    }

    fn is_delim(&self) -> bool {
        matches!(self, b'(' | b')' | b'{' | b'}')
    }

    fn is_punc(&self) -> bool {
        matches!(self, b',' | b';')
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Delimeter {
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
}

impl Decode for Delimeter {
    type Error = LexerError;

    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error> {
        match bytes {
            [b'(', r @ ..] => Ok((r, Some(Delimeter::ParenOpen))),
            [b')', r @ ..] => Ok((r, Some(Delimeter::ParenClose))),
            [b'{', r @ ..] => Ok((r, Some(Delimeter::CurlyOpen))),
            [b'}', r @ ..] => Ok((r, Some(Delimeter::CurlyClose))),
            _ => Ok((bytes, None)),
        }
    }
}

impl Decode for Token {
    type Error = LexerError;

    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error> {
        match bytes {
            [b' ' | b'\n' | b'\t' | b'\r', r @ ..] => return Ok((r, Some(Token::WhiteSpace))),
            [] => return Ok((bytes, Some(Token::Eof))),
            _ => {}
        };

        decode!(bytes, Op);
        decode!(bytes, Keyword);
        decode!(bytes, Arithmetic);
        decode!(bytes, Literal);
        decode!(bytes, Delimeter);

        match bytes {
            [b'=', r @ ..] => return Ok((r, Some(Token::Eq))),
            [b';', r @ ..] => return Ok((r, Some(Token::Semicolon))),
            [b',', r @ ..] => return Ok((r, Some(Token::Comma))),
            _ => {}
        };

        decode!(bytes, Identifier);

        Ok((bytes, None))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
    Let,
    If,
    Else,
    While,
    For,
    Fn,
    Print,
    Return,
}

impl Decode for Keyword {
    type Error = LexerError;

    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error> {
        Ok(match bytes {
            [b'l', b'e', b't', b' ' | b'\n', r @ ..] => (r, Some(Keyword::Let)),
            [b'i', b'f', b' ' | b'\n', r @ ..] => (r, Some(Keyword::If)),
            [b'e', b'l', b's', b'e', b' ' | b'\n', r @ ..] => (r, Some(Keyword::Else)),
            [b'w', b'h', b'i', b'l', b'e', b' ' | b'\n', r @ ..] => (r, Some(Keyword::While)),
            [b'f', b'o', b'r', b' ' | b'\n', r @ ..] => (r, Some(Keyword::For)),
            [b'f', b'n', b' ' | b'\n', r @ ..] => (r, Some(Keyword::Fn)),
            [b'p', b'r', b'i', b'n', b't', b'!', r @ ..] => (r, Some(Keyword::Print)),
            [b'r', b'e', b't', b'u', b'r', b'n', b' ' | b'\n', r @ ..] => {
                (r, Some(Keyword::Return))
            }

            _ => (bytes, None),
        })
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Identifier(pub String);

impl Decode for Identifier {
    type Error = LexerError;

    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error> {
        //The bytes cannot start with a number because it will match the literal check above

        for (index, b) in bytes.iter().enumerate() {
            if b.is_whitespace() || b.is_punc() || b.is_delim() {
                //b cannot be an empty string
                if index == 0 {
                    return Ok((&[], None));
                }

                return Ok((
                    &bytes[index..],
                    Some(Identifier(
                        String::from_utf8(bytes[..index].to_vec())
                            .map_err(|_| LexerError::InvalidUtf8)?,
                    )),
                ));
            }
        }

        Ok((
            &[],
            Some(Identifier(
                String::from_utf8(bytes.to_vec()).map_err(|_| LexerError::InvalidUtf8)?,
            )),
        ))
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Arithmetic {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

impl Decode for Arithmetic {
    type Error = LexerError;
    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error> {
        match bytes {
            [b'+', r @ ..] => Ok((r, Some(Arithmetic::Plus))),
            [b'-', r @ ..] => Ok((r, Some(Arithmetic::Minus))),
            [b'*', r @ ..] => Ok((r, Some(Arithmetic::Mul))),
            [b'/', r @ ..] => Ok((r, Some(Arithmetic::Div))),
            [b'%', r @ ..] => Ok((r, Some(Arithmetic::Mod))),
            _ => Ok((bytes, None)),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Op {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Not,
}

impl Decode for Op {
    type Error = LexerError;

    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error> {
        match bytes {
            [b'=', b'=', r @ ..] => Ok((r, Some(Op::Eq))),
            [b'!', b'=', r @ ..] => Ok((r, Some(Op::Ne))),
            [b'<', b'=', r @ ..] => Ok((r, Some(Op::Le))),
            [b'>', b'=', r @ ..] => Ok((r, Some(Op::Ge))),
            [b'&', b'&', r @ ..] => Ok((r, Some(Op::And))),
            [b'|', b'|', r @ ..] => Ok((r, Some(Op::Or))),
            [b'<', r @ ..] => Ok((r, Some(Op::Lt))),
            [b'>', r @ ..] => Ok((r, Some(Op::Gt))),
            [b'!', r @ ..] => Ok((r, Some(Op::Not))),
            _ => Ok((bytes, None)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
        }
    }
}

impl Eq for Literal {}

impl Decode for Literal {
    type Error = LexerError;

    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error> {
        match bytes {
            [b'"', ref r @ ..] => {
                let mut r = r;
                let mut s = String::new();
                while !r.is_empty() {
                    r = match r {
                        [b'\\', b'"', r @ ..] => {
                            s.push('"');
                            r
                        }
                        [b'\\', b'n', r @ ..] => {
                            s.push('\n');
                            r
                        }
                        [b'"', r @ ..] => return Ok((r, Some(Literal::String(s)))),
                        _ => {
                            s.push(r[0] as char);
                            &r[1..]
                        }
                    }
                }
                Ok((r, None))
            }
            [num @ b'0'..=b'9', r @ ..] => {
                let mut num = String::from(*num as char);

                for (index, b) in r.iter().enumerate() {
                    if b.is_delim() || b.is_whitespace() || b.is_punc() {
                        return Ok((
                            &r[index..],
                            Some(Literal::Number(
                                num.parse().map_err(|_| LexerError::InvalidNumber)?,
                            )),
                        ));
                    }

                    num.push(*b as char);
                }

                Ok((
                    r,
                    Some(Literal::Number(
                        num.parse().map_err(|_| LexerError::InvalidNumber)?,
                    )),
                ))
            }
            [b'f', b'a', b'l', b's', b'e', r @ ..]
                if r.first()
                    .map(|a| a.is_whitespace() || a.is_delim() || a.is_punc())
                    .unwrap_or(true) =>
            {
                Ok((r, Some(Literal::Boolean(false))))
            }
            [b't', b'r', b'u', b'e', r @ ..]
                if r.first()
                    .map(|a| a.is_whitespace() || a.is_delim() || a.is_punc())
                    .unwrap_or(true) =>
            {
                Ok((r, Some(Literal::Boolean(true))))
            }
            _ => Ok((bytes, None)),
        }
    }
}

trait Decode: Sized {
    type Error;

    fn decode(bytes: &[u8]) -> Result<(&[u8], Option<Self>), Self::Error>;
}

pub fn parse(bytes: &[u8]) -> Result<VecDeque<Token>, LexerError> {
    let mut tokens = VecDeque::new();

    let mut bytes = bytes;
    while !bytes.is_empty() {
        let (b, Some(token)) = Token::decode(bytes)? else {
            return Err(LexerError::TokenError);
        };

        bytes = b;
        tokens.push_back(token);
    }

    Ok(tokens)
}

#[cfg(test)]

mod test {

    #[test]
    fn test_assign() {
        use super::*;
        let tokens = parse(b"let a = 1;").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Let),
                Token::Identifier(Identifier("a".to_string())),
                Token::WhiteSpace,
                Token::Eq,
                Token::WhiteSpace,
                Token::Literal(Literal::Number(1.0)),
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn test_reassign() {
        use super::*;
        let tokens = parse(b"a = 1;").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier(Identifier("a".to_string())),
                Token::WhiteSpace,
                Token::Eq,
                Token::WhiteSpace,
                Token::Literal(Literal::Number(1.0)),
                Token::Semicolon,
            ]
        );
    }
}
