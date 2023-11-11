enum Token {
    Keyword(Keyword),
    Arithmetic(Arithmetic),
    Op(Op),
    Literal(Literal),
    Identifer(Identifer),
    Eq,
}

impl Decode for Token {
    fn decode(bytes: &[u8]) -> (&[u8], Option<Self>) {
        match bytes {}
    }
}

enum Keyword {
    Let,
    If,
    Else,
    While,
    For,
    Fn,
}

enum Arithmetic {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

enum Op {
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

enum Literal {}

trait Decode: Sized {
    fn decode(bytes: &[u8]) -> (&[u8], Option<Self>);
}

pub fn parse(bytes: &[u8]) -> Vec<Token> {
    let tokens = Vec::new();

    while !bytes.is_empty() {
        match bytes {
            [b'l', b'e', b't', r @ ..] => r,
        };
    }
}
