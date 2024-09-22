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
    pub literal_value: LiteralValue,
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
            literal_value: literal_value,
        }
    }

    pub fn new_valueless(token_type: TokenType, lexeme: String, line: usize) -> Token {
        Token {
            token_type,
            lexeme,
            line,
            literal_value: LiteralValue::None,
        }
    }
}

#[derive(Debug)]
pub enum LiteralValue {
    Int(i32),
    Float(f64),
    Bool(bool),
    Char(char),
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Ampersand,
    AmpersandEquals,
    And,
    As,
    BangEquals,
    Bar,
    BarEquals,
    BoolType,
    Caret,
    CaretEquals,
    CharType,
    Class,
    Colon,
    Comma,
    DashGreater,
    Def,
    DoubleQuote,
    Dot,
    EOF,
    Else,
    ElseIf,
    Equals,
    EqualsEquals,
    False,
    FloatLiteral,
    FloatType,
    For,
    Greater,
    GreaterEquals,
    GreaterGreater,
    GreaterGreaterEquals,
    Identifier,
    If,
    Implements,
    In,
    IntLiteral,
    IntType,
    Interface,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Less,
    LessEquals,
    LessLess,
    LessLessEquals,
    Let,
    Minus,
    MinusEquals,
    Not,
    NotIn,
    Null,
    Or,
    Percent,
    PercentEquals,
    Plus,
    PlusEquals,
    Pub,
    QuestionMark,
    QuestionMarkDot,
    RightBrace,
    RightBracket,
    RightParen,
    SelfLower,
    SelfUpper,
    SemiColon,
    SingleQuote,
    Slash,
    SlashEquals,
    SlashSlash,
    SlashStar,
    Star,
    StarEquals,
    StringLiteral,
    StringType,
    Tilde,
    True,
    While,
}
