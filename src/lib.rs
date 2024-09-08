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

#[derive(Debug)]
pub struct Token<'a, T> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub line: usize,
    pub literal_value: Option<T>,
}

impl<'a, T> Token<'a, T> {
    pub fn new_literal(
        token_type: TokenType,
        lexeme: &'a str,
        line: usize,
        literal_value: T,
    ) -> Token<T> {
        Token {
            token_type,
            lexeme,
            line,
            literal_value: Some(literal_value),
        }
    }

    pub fn new_valueless(token_type: TokenType, lexeme: &'a str, line: usize) -> Token<T> {
        Token {
            token_type,
            lexeme,
            line,
            literal_value: None,
        }
    }
}

#[derive(Debug)]
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
    TildeEquals,
    //  Equality and Inequality
    EqualsEquals,
    NotEquals,
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
