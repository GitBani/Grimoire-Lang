use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::{Token, TokenType};

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    line: usize,
    offset: usize,
    buffer: Vec<char>,
    keyword_token_type: HashMap<String, TokenType>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source.chars().peekable(),
            tokens: vec![],
            line: 1,
            offset: 0,
            buffer: vec![],
            keyword_token_type: Self::build_keyword_token_type(),
        }
    }

    pub fn tokenize(&mut self) -> &Vec<Token> {
        while self.source.peek() != None {
            self.scan_token();
        }

        self.push_token_valueless(TokenType::EOF);
        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.push_token_valueless(TokenType::LeftParen),
            ')' => self.push_token_valueless(TokenType::RightParen),
            '{' => self.push_token_valueless(TokenType::LeftBrace),
            '}' => self.push_token_valueless(TokenType::RightBrace),
            '[' => self.push_token_valueless(TokenType::LeftBracket),
            ']' => self.push_token_valueless(TokenType::RightBracket),
            ',' => self.push_token_valueless(TokenType::Colon),
            ';' => self.push_token_valueless(TokenType::SemiColon),
            '.' => self.push_token_valueless(TokenType::Dot),

            // '"' => self.scan_string(),
            // '\'' => self.scan_char(),
            '?' => {
                if self.next_char_is('.') {
                    self.push_token_valueless(TokenType::QuestionMarkDot);
                } else {
                    self.push_token_valueless(TokenType::QuestionMark);
                }
            }
            '=' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::EqualsEquals);
                } else {
                    self.push_token_valueless(TokenType::Equals);
                }
            }

            '!' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::BangEquals);
                }
                // else error
            }

            '+' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::PlusEquals);
                } else {
                    self.push_token_valueless(TokenType::Plus);
                }
            }

            '-' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::MinusEquals);
                } else {
                    self.push_token_valueless(TokenType::Minus);
                }
            }

            '*' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::StarEquals);
                } else {
                    self.push_token_valueless(TokenType::Star);
                }
            }

            '/' => {
                if self.next_char_is('/') {
                    self.push_token_valueless(TokenType::SlashSlash);
                    // self.skip_comment();
                } else if self.next_char_is('*') {
                    self.push_token_valueless(TokenType::SlashStar);
                    // self.skip_multiline_comment();
                } else if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::SlashEquals);
                } else {
                    self.push_token_valueless(TokenType::Star);
                }
            }

            '%' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::PercentEquals);
                } else {
                    self.push_token_valueless(TokenType::Percent);
                }
            }

            '>' => {
                if self.next_char_is('>') {
                    if self.next_char_is('=') {
                        self.push_token_valueless(TokenType::GreaterGreaterEquals)
                    } else {
                        self.push_token_valueless(TokenType::GreaterGreater)
                    }
                } else if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::GreaterEquals);
                } else {
                    self.push_token_valueless(TokenType::Greater);
                }
            }

            '<' => {
                if self.next_char_is('<') {
                    if self.next_char_is('=') {
                        self.push_token_valueless(TokenType::LessLessEquals)
                    } else {
                        self.push_token_valueless(TokenType::LessLess)
                    }
                } else if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::LessEquals);
                } else {
                    self.push_token_valueless(TokenType::Less);
                }
            }

            '&' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::AmpersandEquals);
                } else {
                    self.push_token_valueless(TokenType::Ampersand);
                }
            }

            '|' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::BarEquals);
                } else {
                    self.push_token_valueless(TokenType::Bar);
                }
            }

            '^' => {
                if self.next_char_is('=') {
                    self.push_token_valueless(TokenType::Caret);
                } else {
                    self.push_token_valueless(TokenType::CaretEquals);
                }
            }

            '~' => self.push_token_valueless(TokenType::Tilde),

            _ => {
                if c.is_whitespace() {
                    if c == '\n' {
                        self.line += 1;
                    }
                    self.skip_whitespace();
                } else if c.is_alphabetic() {
                    self.scan_identifier_or_keyword();
                } else {
                    //error
                    ()
                }

                // if c.is_numeric() {
                //     self.scan_numeric();
                //     return;
                // }
            }
        }

        self.clear_buffer();
    }

    fn push_token_valueless(&mut self, token_type: TokenType) {
        let lexeme = self.buffer.iter().collect::<String>();
        self.tokens
            .push(Token::new_valueless(token_type, lexeme, self.line));
    }

    fn scan_identifier_or_keyword(&mut self) {
        if let Some(mut next_char) = self.source.peek() {
            while Self::is_identifier_char(*next_char) {
                self.advance();
                match self.source.peek() {
                    Some(c) => next_char = c,
                    None => return,
                }
            }
        }
        let lexeme = self.buffer.iter().collect::<String>();
        let token_type = match self.keyword_token_type.get(&lexeme) {
            Some(keyword_token_type) => *keyword_token_type,
            None => TokenType::Identifier,
        };
        self.tokens
            .push(Token::new_valueless(token_type, lexeme, self.line));
    }

    fn is_identifier_char(c: char) -> bool {
        return c.is_alphanumeric() || c == '_';
    }

    fn skip_whitespace(&mut self) {
        if let Some(mut next_char) = self.source.peek() {
            while (*next_char).is_whitespace() {
                if *next_char == '\n' {
                    self.line += 1;
                }
                self.advance();
                match self.source.peek() {
                    Some(c) => next_char = c,
                    None => return,
                }
            }
        }
    }

    fn next_char_is(&mut self, expected_char: char) -> bool {
        match self.source.peek() {
            Some(c) => {
                if *c == expected_char {
                    self.advance();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source.next().unwrap();
        self.buffer.push(c);
        self.offset += 1;
        c
    }

    fn clear_buffer(&mut self) {
        self.buffer = vec![]
    }

    fn build_keyword_token_type() -> HashMap<String, TokenType> {
        HashMap::from([
            (String::from("and"), TokenType::And),
            (String::from("or"), TokenType::Or),
            (String::from("not"), TokenType::Not),
            (String::from("in"), TokenType::In),
            (String::from("true"), TokenType::True),
            (String::from("false"), TokenType::False),
            (String::from("int"), TokenType::IntType),
            (String::from("float"), TokenType::FloatType),
            (String::from("bool"), TokenType::BoolType),
            (String::from("char"), TokenType::CharType),
            (String::from("string"), TokenType::StringType),
            (String::from("null"), TokenType::Null),
            (String::from("let"), TokenType::Let),
            (String::from("as"), TokenType::As),
            (String::from("if"), TokenType::If),
            (String::from("else"), TokenType::Else),
            (String::from("while"), TokenType::While),
            (String::from("for"), TokenType::For),
            (String::from("def"), TokenType::Def),
            (String::from("class"), TokenType::Class),
            (String::from("interface"), TokenType::Interface),
            (String::from("pub"), TokenType::Pub),
            (String::from("self"), TokenType::SelfLower),
            (String::from("Self"), TokenType::SelfUpper),
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_token() {
        let source = "((]}+=+ 
          &&=>>   <<=%;";
        let mut lexer = Lexer::new(source);
        let expected = vec![
            Token::new_valueless(TokenType::LeftParen, String::from("("), 1),
            Token::new_valueless(TokenType::LeftParen, String::from("("), 1),
            Token::new_valueless(TokenType::RightBracket, String::from("]"), 1),
            Token::new_valueless(TokenType::RightBrace, String::from("}"), 1),
            Token::new_valueless(TokenType::PlusEquals, String::from("+="), 1),
            Token::new_valueless(TokenType::Plus, String::from("+"), 1),
            Token::new_valueless(TokenType::Ampersand, String::from("&"), 2),
            Token::new_valueless(TokenType::AmpersandEquals, String::from("&="), 2),
            Token::new_valueless(TokenType::GreaterGreater, String::from(">>"), 2),
            Token::new_valueless(TokenType::LessLessEquals, String::from("<<="), 2),
            Token::new_valueless(TokenType::Percent, String::from("%"), 2),
            Token::new_valueless(TokenType::SemiColon, String::from(";"), 2),
            Token::new_valueless(TokenType::EOF, String::from(""), 2),
        ];
        assert_eq!(*lexer.tokenize(), expected);
    }
}
