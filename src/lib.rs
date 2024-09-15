use std::backtrace;

pub mod lexer;

pub struct Config {
    pub path: String,
    pub flags: Vec<String>,
}

impl Config {
    pub fn build(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("Missing file path.");
        }

        let path = args[1].clone();

        Ok(Config {
            path,
            flags: vec![], // Todo: implement compiler flags
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub literal_value: Option<LiteralValue>,
}

impl<'a> Token {
    pub fn new_literal(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        literal_value: LiteralValue,
    ) -> Token {
        Token {
            token_type,
            lexeme,
            line,
            literal_value: Some(literal_value),
        }
    }

    pub fn new_valueless(token_type: TokenType, lexeme: String, line: usize) -> Token {
        Token {
            token_type,
            lexeme,
            line,
            literal_value: None,
        }
    }
}

#[derive(Debug)]
pub enum LiteralValue {
    Int(i32),
    Float(f64),
    Bool(bool),
    None,
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralValue::Int(a), LiteralValue::Int(b)) => a == b,
            (LiteralValue::Float(a), LiteralValue::Float(b)) => (a - b).abs() < f64::EPSILON,
            (LiteralValue::Bool(a), LiteralValue::Bool(b)) => a == b,
            (LiteralValue::None, LiteralValue::None) => true,
            _ => false,
        }
    }
}

impl Eq for LiteralValue {}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    // Punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Dot,
    Comma,
    SemiColon,
    Colon,
    SingleQuote,
    DoubleQuote,
    // Operators
    //  Arithmetic
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    //  Bitwise
    Ampersand,
    Bar,
    Caret,
    GreaterGreater,
    LessLess,
    Tilde,
    //  Assignment
    Equals,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    AmpersandEquals,
    BarEquals,
    CaretEquals,
    GreaterGreaterEquals,
    LessLessEquals,
    //  Equality and Inequality
    EqualsEquals,
    BangEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    //  Boolean
    And,
    Or,
    Not,
    In,
    NotIn,
    // Comments
    SlashSlash,
    SlashStar,
    // Literals
    True,
    False,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    // Types
    IntType,
    FloatType,
    BoolType,
    StrType,
    // Other
    Null,
    Identifier,
    Let,
    As,
    If,
    ElseIf,
    Else,
    While,
    For,
    Def,
    RightArrow,
    Class,
    Implements,
    Interface,
    Pub,
    Static,
    EOF,
}
