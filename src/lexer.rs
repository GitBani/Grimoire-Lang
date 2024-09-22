use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::{LiteralValue, Token, TokenType};

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

    pub fn get_tokens(&mut self) -> &Vec<Token> {
        if self.tokens.len() == 0 {
            self.tokenize();
        }
        &self.tokens
    }

    fn tokenize(&mut self) {
        while self.source.peek() != None {
            self.scan_token();
        }

        self.push_token_valueless(TokenType::EOF);
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
            '\'' => self.scan_char(),

            '?' => {
                if self.consume_if_next_char_equals('.') {
                    self.push_token_valueless(TokenType::QuestionMarkDot);
                } else {
                    self.push_token_valueless(TokenType::QuestionMark);
                }
            }
            '=' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::EqualsEquals);
                } else {
                    self.push_token_valueless(TokenType::Equals);
                }
            }

            '!' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::BangEquals);
                }
                // else error
            }

            '+' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::PlusEquals);
                } else {
                    self.push_token_valueless(TokenType::Plus);
                }
            }

            '-' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::MinusEquals);
                } else if self.consume_if_next_char_equals('>') {
                    self.push_token_valueless(TokenType::DashGreater);
                } else if let Some(c) = self.source.peek() {
                    if c.is_numeric() {
                        self.advance();
                        self.scan_numeric();
                    } else {
                        self.push_token_valueless(TokenType::Minus);
                    }
                }
            }

            '*' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::StarEquals);
                } else {
                    self.push_token_valueless(TokenType::Star);
                }
            }

            '/' => {
                if self.consume_if_next_char_equals('/') {
                    self.push_token_valueless(TokenType::SlashSlash);
                    // self.skip_comment();
                } else if self.consume_if_next_char_equals('*') {
                    self.push_token_valueless(TokenType::SlashStar);
                    // self.skip_multiline_comment();
                } else if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::SlashEquals);
                } else {
                    self.push_token_valueless(TokenType::Star);
                }
            }

            '%' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::PercentEquals);
                } else {
                    self.push_token_valueless(TokenType::Percent);
                }
            }

            '>' => {
                if self.consume_if_next_char_equals('>') {
                    if self.consume_if_next_char_equals('=') {
                        self.push_token_valueless(TokenType::GreaterGreaterEquals)
                    } else {
                        self.push_token_valueless(TokenType::GreaterGreater)
                    }
                } else if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::GreaterEquals);
                } else {
                    self.push_token_valueless(TokenType::Greater);
                }
            }

            '<' => {
                if self.consume_if_next_char_equals('<') {
                    if self.consume_if_next_char_equals('=') {
                        self.push_token_valueless(TokenType::LessLessEquals)
                    } else {
                        self.push_token_valueless(TokenType::LessLess)
                    }
                } else if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::LessEquals);
                } else {
                    self.push_token_valueless(TokenType::Less);
                }
            }

            '&' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::AmpersandEquals);
                } else {
                    self.push_token_valueless(TokenType::Ampersand);
                }
            }

            '|' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::BarEquals);
                } else {
                    self.push_token_valueless(TokenType::Bar);
                }
            }

            '^' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_token_valueless(TokenType::Caret);
                } else {
                    self.push_token_valueless(TokenType::CaretEquals);
                }
            }

            '~' => self.push_token_valueless(TokenType::Tilde),

            _ => {
                if c.is_whitespace() {
                    self.skip_whitespace(c);
                } else if c.is_alphabetic() {
                    self.scan_identifier_or_keyword();
                } else if c.is_numeric() {
                    self.scan_numeric();
                } else {
                    //error
                    ()
                }
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
                    None => break,
                }
            }
        }

        let lexeme = self.buffer.iter().collect::<String>();
        let token_type = match self.keyword_token_type.get(&lexeme) {
            Some(keyword_token_type) => *keyword_token_type,
            None => TokenType::Identifier,
        };

        self.tokens.push(match token_type {
            TokenType::True => {
                Token::new_literal(token_type, lexeme, self.line, LiteralValue::Bool(true))
            }
            TokenType::False => {
                Token::new_literal(token_type, lexeme, self.line, LiteralValue::Bool(false))
            }
            _ => Token::new_valueless(token_type, lexeme, self.line),
        });
    }

    fn is_identifier_char(c: char) -> bool {
        return c.is_alphanumeric() || c == '_';
    }

    fn scan_char(&mut self) {
        if let Some(_) = self.source.peek() {
            let value = self.advance();
            if !self.consume_if_next_char_equals('\'') {
                // error
            }
            let lexeme = self.buffer.iter().collect::<String>();
            self.tokens.push(Token::new_literal(
                TokenType::CharType,
                lexeme,
                self.line,
                LiteralValue::Char(value),
            ))
        } else {
            // error
        }
    }

    fn scan_numeric(&mut self) {
        // at this point, buffer either contains a single digit char or '-' and a digit char
        let mut value = self
            .buffer
            .last()
            .expect(&format!(
                "In scan_numeric at {}:{}, buffer is: {:?}",
                self.line, self.offset, self.buffer
            ))
            .to_digit(10)
            .unwrap();
        if let Some(mut next_char) = self.source.peek() {
            while next_char.is_numeric() {
                value = value * 10 + next_char.to_digit(10).unwrap();

                self.advance();
                match self.source.peek() {
                    Some(c) => next_char = c,
                    None => break,
                }
            }
        }

        if self.consume_if_next_char_equals('.') {
            // number being scanned is a float
            // fractional part calculated as an int to avoid floating point error
            let mut fractional_part = 0;
            let mut num_decimals = 0;
            if let Some(mut next_char) = self.source.peek() {
                // error if not at least 1 decimal digit
                while next_char.is_numeric() {
                    num_decimals += 1;
                    fractional_part = fractional_part * 10 + next_char.to_digit(10).unwrap();

                    self.advance();
                    match self.source.peek() {
                        Some(c) => next_char = c,
                        None => break,
                    }
                }

                let mut value =
                    value as f64 + fractional_part as f64 / 10_i32.pow(num_decimals) as f64;
                if *self.buffer.first().expect(&format!(
                    "In scan_numeric at {}:{}, buffer is: {:?}",
                    self.line, self.offset, self.buffer
                )) == '-'
                {
                    value *= -1.0;
                }

                let lexeme = self.buffer.iter().collect::<String>();
                self.tokens.push(Token::new_literal(
                    TokenType::FloatLiteral,
                    lexeme,
                    self.line,
                    LiteralValue::Float(value),
                ));
            } else {
                // error since no decimal digits after '.'
            }
        } else {
            // number being scanned is an int
            let mut value = value as i32;
            if *self.buffer.first().expect(&format!(
                "In scan_numeric at {}:{}, buffer is: {:?}",
                self.line, self.offset, self.buffer
            )) == '-'
            {
                value *= -1;
            }

            let lexeme = self.buffer.iter().collect::<String>();
            self.tokens.push(Token::new_literal(
                TokenType::IntLiteral,
                lexeme,
                self.line,
                LiteralValue::Int(value),
            ));
        }
    }

    fn skip_whitespace(&mut self, initial_char: char) {
        if initial_char == '\n' {
            self.line += 1;
            self.offset = 1;
        }
        if let Some(mut next_char) = self.source.peek() {
            while next_char.is_whitespace() {
                if *next_char == '\n' {
                    self.line += 1;
                    self.offset = 1;
                }

                self.advance();
                match self.source.peek() {
                    Some(c) => next_char = c,
                    None => return,
                }
            }
        }
    }

    fn consume_if_next_char_equals(&mut self, expected_char: char) -> bool {
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
        // dbg!(self.source.next());
        let c = self.source.next().expect(&format!(
            "In advance at {}:{}, buffer is: {:?}",
            self.line, self.offset, self.buffer
        ));
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
    fn test_operators() {
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

        assert_eq!(*lexer.get_tokens(), expected);
    }

    #[test]
    fn test_chars_and_strings() {
        let source = "'a' 'B' 'c' 'D'
        '1' '2' '3' '4'";

        let mut lexer = Lexer::new(source);
        let expected = vec![
            Token::new_literal(
                TokenType::CharType,
                String::from("'a'"),
                1,
                LiteralValue::Char('a'),
            ),
            Token::new_literal(
                TokenType::CharType,
                String::from("'B'"),
                1,
                LiteralValue::Char('B'),
            ),
            Token::new_literal(
                TokenType::CharType,
                String::from("'c'"),
                1,
                LiteralValue::Char('c'),
            ),
            Token::new_literal(
                TokenType::CharType,
                String::from("'D'"),
                1,
                LiteralValue::Char('D'),
            ),
            Token::new_literal(
                TokenType::CharType,
                String::from("'1'"),
                2,
                LiteralValue::Char('1'),
            ),
            Token::new_literal(
                TokenType::CharType,
                String::from("'2'"),
                2,
                LiteralValue::Char('2'),
            ),
            Token::new_literal(
                TokenType::CharType,
                String::from("'3'"),
                2,
                LiteralValue::Char('3'),
            ),
            Token::new_literal(
                TokenType::CharType,
                String::from("'4'"),
                2,
                LiteralValue::Char('4'),
            ),
            Token::new_valueless(TokenType::EOF, String::from(""), 2),
        ];

        assert_eq!(*lexer.get_tokens(), expected);
    }

    #[test]
    fn test_keywords_and_identifiers() {
        let source = "hello world let x as true else if null as false
        pub in england
        self as Self be your true self
        float in int
        take your time";

        let mut lexer = Lexer::new(source);
        let expected = vec![
            Token::new_valueless(TokenType::Identifier, String::from("hello"), 1),
            Token::new_valueless(TokenType::Identifier, String::from("world"), 1),
            Token::new_valueless(TokenType::Let, String::from("let"), 1),
            Token::new_valueless(TokenType::Identifier, String::from("x"), 1),
            Token::new_valueless(TokenType::As, String::from("as"), 1),
            Token::new_literal(
                TokenType::True,
                String::from("true"),
                1,
                LiteralValue::Bool(true),
            ),
            Token::new_valueless(TokenType::Else, String::from("else"), 1),
            Token::new_valueless(TokenType::If, String::from("if"), 1),
            Token::new_valueless(TokenType::Null, String::from("null"), 1),
            Token::new_valueless(TokenType::As, String::from("as"), 1),
            Token::new_literal(
                TokenType::False,
                String::from("false"),
                1,
                LiteralValue::Bool(false),
            ),
            Token::new_valueless(TokenType::Pub, String::from("pub"), 2),
            Token::new_valueless(TokenType::In, String::from("in"), 2),
            Token::new_valueless(TokenType::Identifier, String::from("england"), 2),
            Token::new_valueless(TokenType::SelfLower, String::from("self"), 3),
            Token::new_valueless(TokenType::As, String::from("as"), 3),
            Token::new_valueless(TokenType::SelfUpper, String::from("Self"), 3),
            Token::new_valueless(TokenType::Identifier, String::from("be"), 3),
            Token::new_valueless(TokenType::Identifier, String::from("your"), 3),
            Token::new_literal(
                TokenType::True,
                String::from("true"),
                3,
                LiteralValue::Bool(true),
            ),
            Token::new_valueless(TokenType::SelfLower, String::from("self"), 3),
            Token::new_valueless(TokenType::FloatType, String::from("float"), 4),
            Token::new_valueless(TokenType::In, String::from("in"), 4),
            Token::new_valueless(TokenType::IntType, String::from("int"), 4),
            Token::new_valueless(TokenType::Identifier, String::from("take"), 5),
            Token::new_valueless(TokenType::Identifier, String::from("your"), 5),
            Token::new_valueless(TokenType::Identifier, String::from("time"), 5),
            Token::new_valueless(TokenType::EOF, String::from(""), 5),
        ];

        let result = lexer.get_tokens();
        for i in 0..expected.len() - 1 {
            if expected[i] != result[i] {
                dbg!(&expected[i]);
                dbg!(&result[i]);
                break;
            }
        }

        assert_eq!(*lexer.get_tokens(), expected);
    }

    #[test]
    fn test_numerics() {
        let source = "99 -6543 9 0 -1234567
        88.8 -106.12
        34291.123456 -0.15325 100000000.3333 2345654.346765 -2345654.346765
        ";

        let mut lexer = Lexer::new(source);
        let expected = vec![
            Token::new_literal(
                TokenType::IntLiteral,
                String::from("99"),
                1,
                LiteralValue::Int(99),
            ),
            Token::new_literal(
                TokenType::IntLiteral,
                String::from("-6543"),
                1,
                LiteralValue::Int(-6543),
            ),
            Token::new_literal(
                TokenType::IntLiteral,
                String::from("9"),
                1,
                LiteralValue::Int(9),
            ),
            Token::new_literal(
                TokenType::IntLiteral,
                String::from("0"),
                1,
                LiteralValue::Int(0),
            ),
            Token::new_literal(
                TokenType::IntLiteral,
                String::from("-1234567"),
                1,
                LiteralValue::Int(-1234567),
            ),
            Token::new_literal(
                TokenType::FloatLiteral,
                String::from("88.8"),
                2,
                LiteralValue::Float(88.8),
            ),
            Token::new_literal(
                TokenType::FloatLiteral,
                String::from("-106.12"),
                2,
                LiteralValue::Float(-106.12),
            ),
            Token::new_literal(
                TokenType::FloatLiteral,
                String::from("34291.123456"),
                3,
                LiteralValue::Float(34291.123456),
            ),
            Token::new_literal(
                TokenType::FloatLiteral,
                String::from("-0.15325"),
                3,
                LiteralValue::Float(-0.15325),
            ),
            Token::new_literal(
                TokenType::FloatLiteral,
                String::from("100000000.3333"),
                3,
                LiteralValue::Float(100000000.3333),
            ),
            Token::new_literal(
                TokenType::FloatLiteral,
                String::from("2345654.346765"),
                3,
                LiteralValue::Float(2345654.346765),
            ),
            Token::new_literal(
                TokenType::FloatLiteral,
                String::from("-2345654.346765"),
                3,
                LiteralValue::Float(-2345654.346765),
            ),
            Token::new_valueless(TokenType::EOF, String::from(""), 4),
        ];

        assert_eq!(*lexer.get_tokens(), expected);
    }
}
