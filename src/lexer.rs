use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::errors;
use crate::token::{LiteralValue, Token, TokenType};

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
            keyword_token_type: Self::build_keyword_token_type_map(),
        }
    }

    pub fn get_tokens(&mut self) -> &Vec<Token> {
        if self.tokens.len() == 0 {
            self.tokenize();
        }
        &self.tokens
    }

    fn tokenize(&mut self) {
        while self.peek_next() != None {
            self.scan_token();
        }

        self.push_valueless_token(TokenType::EOF);
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.push_valueless_token(TokenType::LeftParen),
            ')' => self.push_valueless_token(TokenType::RightParen),
            '{' => self.push_valueless_token(TokenType::LeftBrace),
            '}' => self.push_valueless_token(TokenType::RightBrace),
            '[' => self.push_valueless_token(TokenType::LeftBracket),
            ']' => self.push_valueless_token(TokenType::RightBracket),
            ',' => self.push_valueless_token(TokenType::Comma),
            ':' => self.push_valueless_token(TokenType::Colon),
            ';' => self.push_valueless_token(TokenType::SemiColon),
            '.' => self.push_valueless_token(TokenType::Dot),

            '"' => self.scan_string(),
            '\'' => self.scan_char(),

            '?' => {
                if self.consume_if_next_char_equals('.') {
                    self.push_valueless_token(TokenType::QuestionMarkDot);
                } else {
                    self.push_valueless_token(TokenType::QuestionMark);
                }
            }
            '=' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::EqualsEquals);
                } else {
                    self.push_valueless_token(TokenType::Equals);
                }
            }

            '!' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::NotEquals);
                } else {
                    self.push_valueless_token(TokenType::Not);
                }
            }

            '+' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::PlusEquals);
                } else {
                    self.push_valueless_token(TokenType::Plus);
                }
            }

            '-' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::MinusEquals);
                } else if self.consume_if_next_char_equals('>') {
                    self.push_valueless_token(TokenType::DashGreater);
                } else if let Some(c) = self.peek_next() {
                    if c.is_numeric() {
                        self.advance();
                        self.scan_numeric();
                    } else {
                        self.push_valueless_token(TokenType::Minus);
                    }
                }
            }

            '*' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::StarEquals);
                } else {
                    self.push_valueless_token(TokenType::Star);
                }
            }

            '/' => {
                if self.consume_if_next_char_equals('/') {
                    self.skip_comment();
                } else if self.consume_if_next_char_equals('*') {
                    self.skip_multiline_comment();
                } else if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::SlashEquals);
                } else {
                    self.push_valueless_token(TokenType::Slash);
                }
            }

            '%' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::PercentEquals);
                } else {
                    self.push_valueless_token(TokenType::Percent);
                }
            }

            '>' => {
                if self.consume_if_next_char_equals('>') {
                    if self.consume_if_next_char_equals('=') {
                        self.push_valueless_token(TokenType::GreaterGreaterEquals)
                    } else {
                        self.push_valueless_token(TokenType::GreaterGreater)
                    }
                } else if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::GreaterEquals);
                } else {
                    self.push_valueless_token(TokenType::Greater);
                }
            }

            '<' => {
                if self.consume_if_next_char_equals('<') {
                    if self.consume_if_next_char_equals('=') {
                        self.push_valueless_token(TokenType::LessLessEquals)
                    } else {
                        self.push_valueless_token(TokenType::LessLess)
                    }
                } else if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::LessEquals);
                } else {
                    self.push_valueless_token(TokenType::Less);
                }
            }

            '&' => {
                if self.consume_if_next_char_equals('&') {
                    self.push_valueless_token(TokenType::AmpersandAmpersand);
                } else if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::AmpersandEquals);
                } else {
                    self.push_valueless_token(TokenType::Ampersand);
                }
            }

            '|' => {
                if self.consume_if_next_char_equals('|') {
                    self.push_valueless_token(TokenType::BarBar);
                } else if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::BarEquals);
                } else {
                    self.push_valueless_token(TokenType::Bar);
                }
            }

            '^' => {
                if self.consume_if_next_char_equals('=') {
                    self.push_valueless_token(TokenType::Caret);
                } else {
                    self.push_valueless_token(TokenType::CaretEquals);
                }
            }

            '~' => self.push_valueless_token(TokenType::Tilde),

            _ => {
                if c.is_whitespace() {
                    self.skip_whitespace(c);
                } else if c.is_alphabetic() || c == '_' {
                    self.scan_identifier_or_keyword();
                } else if c.is_numeric() {
                    self.scan_numeric();
                } else {
                    errors::report_syntax_error(
                        &format!("Unrecognized char: `{c}`"),
                        self.line,
                        self.offset,
                    );
                }
            }
        }

        self.clear_buffer();
    }

    //-- Scanning helpers -----------------------------------------------------------------------------------------------------------------------------------------
    fn scan_identifier_or_keyword(&mut self) {
        if let Some(mut next_char) = self.peek_next() {
            while Self::is_identifier_char(next_char) {
                self.advance();
                match self.peek_next() {
                    Some(c) => next_char = c,
                    None => break,
                }
            }
        }

        let lexeme: String = self.buffer.iter().collect();
        let position = self.offset - lexeme.len() + 1;
        let token_type = match self.keyword_token_type.get(&lexeme) {
            Some(keyword_token_type) => *keyword_token_type,
            None => TokenType::Identifier,
        };

        self.tokens.push(match token_type {
            TokenType::True => Token::new_literal(
                token_type,
                lexeme,
                self.line,
                position,
                LiteralValue::Bool(true),
            ),
            TokenType::False => Token::new_literal(
                token_type,
                lexeme,
                self.line,
                position,
                LiteralValue::Bool(false),
            ),
            _ => Token::new_valueless(token_type, lexeme, self.line, position),
        });
    }

    fn is_identifier_char(c: char) -> bool {
        return c.is_alphanumeric() || c == '_';
    }

    fn scan_string(&mut self) {
        let starting_line = self.line;
        let starting_column = self.offset + 1;

        if let Some(mut next_char) = self.peek_next() {
            while !self.consume_if_next_char_equals('"') {
                if next_char == '\n' {
                    self.consume_newline();
                } else {
                    self.advance();
                }

                match self.peek_next() {
                    Some(c) => next_char = c,
                    None => errors::report_syntax_error(
                        "Unterminated string",
                        starting_line,
                        starting_column,
                    ),
                }
            }

            let value = self.buffer[1..self.buffer.len() - 1] // cut off quotations
                .to_vec()
                .iter()
                .collect();
            self.push_valued_token(TokenType::StringLiteral, LiteralValue::String(value));
        } else {
            errors::report_syntax_error("Unterminated string", starting_line, starting_column);
        }
    }

    fn scan_char(&mut self) {
        if let Some(_) = self.peek_next() {
            let value = self.advance(); // note: this allows the char ''' (single quote w/o escaping (lowkey kinda goated?))
            if !self.consume_if_next_char_equals('\'') {
                errors::report_syntax_error("Unterminated char", self.line, self.offset);
            }
            self.push_valued_token(TokenType::CharLiteral, LiteralValue::Char(value));
        } else {
            errors::report_syntax_error("Unterminated char", self.line, self.offset);
        }
    }

    fn scan_numeric(&mut self) {
        // at this point, buffer either contains a single digit char or '-' and a digit char
        let mut value = self
            .buffer
            .last()
            .expect(&format!(
                "In scan_numeric at {}:{}, buffer is: {:#?} but expected it to contain a digit (or negative sign then digit)",
                self.line, self.offset, self.buffer
            ))
            .to_digit(10)
            .unwrap();

        if let Some(mut next_char) = self.peek_next() {
            while next_char.is_numeric() {
                value = value * 10 + next_char.to_digit(10).unwrap();

                self.advance();
                match self.peek_next() {
                    Some(c) => next_char = c,
                    None => break,
                }
            }
        }

        if self.consume_if_next_char_equals('.') {
            self.scan_fractional(value);
        } else {
            let mut value = value as i32;
            if *self.buffer.first().unwrap() == '-' {
                value *= -1;
            }

            self.push_valued_token(TokenType::IntLiteral, LiteralValue::Int(value));
        }
    }

    fn scan_fractional(&mut self, integer_part: u32) {
        // fractional part calculated as an int to avoid floating point error
        let mut fractional_part = 0;
        let mut num_decimals = 0;

        if let Some(mut next_char) = self.peek_next() {
            while next_char.is_numeric() {
                num_decimals += 1;
                fractional_part = fractional_part * 10 + next_char.to_digit(10).unwrap();

                self.advance();
                match self.peek_next() {
                    Some(c) => next_char = c,
                    None => break,
                }
            }
        }

        let mut value =
            integer_part as f64 + fractional_part as f64 / 10_i32.pow(num_decimals) as f64;
        if *self.buffer.first().unwrap() == '-' {
            value *= -1.0;
        }

        self.push_valued_token(TokenType::FloatLiteral, LiteralValue::Float(value));
    }

    fn skip_whitespace(&mut self, initial_char: char) {
        if initial_char == '\n' {
            println!("Reached on line {} col {}", self.line, self.offset);
            self.process_newline();
        }
        if let Some(mut next_char) = self.peek_next() {
            while next_char.is_whitespace() {
                dbg!(next_char);
                if next_char == '\n' {
                    println!("Inner reached on line {} col {}", self.line, self.offset);
                    self.consume_newline();
                } else {
                    self.advance();
                }

                match self.peek_next() {
                    Some(c) => next_char = c,
                    None => return,
                }
            }
        }
    }

    fn skip_comment(&mut self) {
        if let Some(mut next_char) = self.peek_next() {
            // let the next iteration of `scan_token` consume the newline so that `process_newline` is called
            while next_char != '\n' {
                self.advance();
                match self.peek_next() {
                    Some(c) => next_char = c,
                    None => return,
                }
            }
        }
    }

    fn skip_multiline_comment(&mut self) {
        let starting_line = self.line;
        let starting_column = self.offset + 1;

        if let Some(mut next_char) = self.peek_next() {
            let mut terminated = false;
            while !terminated {
                if next_char == '*' {
                    self.advance();
                    match self.peek_next() {
                        Some('/') => {
                            self.advance();
                            terminated = true;
                        }
                        Some(c) => next_char = c,
                        None => errors::report_syntax_error(
                            "Unterminated multiline comment",
                            starting_line,
                            starting_column,
                        ),
                    }
                    continue;
                }

                if next_char == '\n' {
                    self.consume_newline();
                } else {
                    self.advance();
                }

                match self.peek_next() {
                    Some(c) => next_char = c,
                    None => errors::report_syntax_error(
                        "Unterminated multiline comment",
                        starting_line,
                        starting_column,
                    ),
                }
            }
        } else {
            errors::report_syntax_error(
                "Unterminated multiline comment",
                starting_line,
                starting_column,
            );
        }
    }

    //-- Iteration tools ------------------------------------------------------------------------------------------------------------------------------------------
    fn push_valueless_token(&mut self, token_type: TokenType) {
        let lexeme: String = self.buffer.iter().collect();
        let len = lexeme.len();
        self.tokens.push(Token::new_valueless(
            token_type,
            lexeme,
            self.line,
            self.offset - len + 1,
        ));
    }

    fn push_valued_token(&mut self, token_type: TokenType, value: LiteralValue) {
        let lexeme: String = self.buffer.iter().collect();
        let len = lexeme.len();
        self.tokens.push(Token::new_literal(
            token_type,
            lexeme,
            self.line,
            self.offset - len + 1,
            value,
        ));
    }

    fn consume_newline(&mut self) {
        self.advance();
        self.process_newline();
    }

    fn process_newline(&mut self) {
        self.line += 1;
        self.offset = 0;
    }

    fn consume_if_next_char_equals(&mut self, expected: char) -> bool {
        match self.peek_next() {
            Some(c) => {
                if c == expected {
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
        let c = self.source.next().expect(&format!(
            "In advance at {}:{}, buffer is: {:?}",
            self.line, self.offset, self.buffer
        ));
        self.buffer.push(c);
        self.offset += 1;
        c
    }

    fn peek_next(&mut self) -> Option<char> {
        return self.source.peek().copied();
    }

    fn clear_buffer(&mut self) {
        self.buffer = vec![]
    }

    //-- Misc -----------------------------------------------------------------------------------------------------------------------------------------------------
    fn build_keyword_token_type_map() -> HashMap<String, TokenType> {
        HashMap::from([
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

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_operators() {
//         let source = "((]}+=+
//           &&&=>>   <<=%;";

//         let mut lexer = Lexer::new(source);
//         let expected = vec![
//             Token::new_valueless(TokenType::LeftParen, String::from("("), 1),
//             Token::new_valueless(TokenType::LeftParen, String::from("("), 1),
//             Token::new_valueless(TokenType::RightBracket, String::from("]"), 1),
//             Token::new_valueless(TokenType::RightBrace, String::from("}"), 1),
//             Token::new_valueless(TokenType::PlusEquals, String::from("+="), 1),
//             Token::new_valueless(TokenType::Plus, String::from("+"), 1),
//             Token::new_valueless(TokenType::AmpersandAmpersand, String::from("&&"), 2),
//             Token::new_valueless(TokenType::AmpersandEquals, String::from("&="), 2),
//             Token::new_valueless(TokenType::GreaterGreater, String::from(">>"), 2),
//             Token::new_valueless(TokenType::LessLessEquals, String::from("<<="), 2),
//             Token::new_valueless(TokenType::Percent, String::from("%"), 2),
//             Token::new_valueless(TokenType::SemiColon, String::from(";"), 2),
//             Token::new_valueless(TokenType::EOF, String::from(""), 2),
//         ];

//         assert_eq!(*lexer.get_tokens(), expected);
//     }

//     #[test]
//     fn test_chars_and_strings() {
//         let source = "'a' 'B' 'c' 'D'
//         '1' '2' '3' '4'
//         \"\" \"'static\" \"on one line
//         now on the other
//         and another, wow!\" \"888
//         777 64 \"
//         ";

//         let mut lexer = Lexer::new(source);
//         let expected = vec![
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'a'"),
//                 1,
//                 LiteralValue::Char('a'),
//             ),
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'B'"),
//                 1,
//                 LiteralValue::Char('B'),
//             ),
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'c'"),
//                 1,
//                 LiteralValue::Char('c'),
//             ),
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'D'"),
//                 1,
//                 LiteralValue::Char('D'),
//             ),
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'1'"),
//                 2,
//                 LiteralValue::Char('1'),
//             ),
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'2'"),
//                 2,
//                 LiteralValue::Char('2'),
//             ),
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'3'"),
//                 2,
//                 LiteralValue::Char('3'),
//             ),
//             Token::new_literal(
//                 TokenType::CharLiteral,
//                 String::from("'4'"),
//                 2,
//                 LiteralValue::Char('4'),
//             ),
//             Token::new_literal(
//                 TokenType::StringLiteral,
//                 String::from("\"\""),
//                 3,
//                 LiteralValue::String(String::from("")),
//             ),
//             Token::new_literal(
//                 TokenType::StringLiteral,
//                 String::from("\"'static\""),
//                 3,
//                 LiteralValue::String(String::from("'static")),
//             ),
//             Token::new_literal(
//                 TokenType::StringLiteral,
//                 String::from(
//                     "\"on one line
//         now on the other
//         and another, wow!\"",
//                 ),
//                 5, // parsed as last line it's on, is this bad?
//                 LiteralValue::String(String::from(
//                     "on one line
//         now on the other
//         and another, wow!",
//                 )),
//             ),
//             Token::new_literal(
//                 TokenType::StringLiteral,
//                 String::from(
//                     "\"888
//         777 64 \"",
//                 ),
//                 6, // parsed as last line it's on, is this bad?
//                 LiteralValue::String(String::from(
//                     "888
//         777 64 ",
//                 )),
//             ),
//             Token::new_valueless(TokenType::EOF, String::from(""), 7),
//         ];

//         for i in 0..lexer.get_tokens().len() {
//             if lexer.get_tokens()[i] != expected[i] {
//                 dbg!(&lexer.get_tokens()[i]);
//                 dbg!(&expected[i]);
//             }
//         }

//         assert_eq!(*lexer.get_tokens(), expected);
//     }

//     #[test]
//     fn test_keywords_and_identifiers() {
//         let source = "hello world let x as true else if null as false
//         pub /* hello */ in england && !in france
//         self as Self be your true self
//         float in int
//         take your time
//         /**
//          *
//          *
//          *
//          *
//          *
//          */
//         eof";

//         let mut lexer = Lexer::new(source);
//         let expected = vec![
//             Token::new_valueless(TokenType::Identifier, String::from("hello"), 1),
//             Token::new_valueless(TokenType::Identifier, String::from("world"), 1),
//             Token::new_valueless(TokenType::Let, String::from("let"), 1),
//             Token::new_valueless(TokenType::Identifier, String::from("x"), 1),
//             Token::new_valueless(TokenType::As, String::from("as"), 1),
//             Token::new_literal(
//                 TokenType::True,
//                 String::from("true"),
//                 1,
//                 LiteralValue::Bool(true),
//             ),
//             Token::new_valueless(TokenType::Else, String::from("else"), 1),
//             Token::new_valueless(TokenType::If, String::from("if"), 1),
//             Token::new_valueless(TokenType::Null, String::from("null"), 1),
//             Token::new_valueless(TokenType::As, String::from("as"), 1),
//             Token::new_literal(
//                 TokenType::False,
//                 String::from("false"),
//                 1,
//                 LiteralValue::Bool(false),
//             ),
//             Token::new_valueless(TokenType::Pub, String::from("pub"), 2),
//             Token::new_valueless(TokenType::In, String::from("in"), 2),
//             Token::new_valueless(TokenType::Identifier, String::from("england"), 2),
//             Token::new_valueless(TokenType::AmpersandAmpersand, String::from("&&"), 2),
//             Token::new_valueless(TokenType::Not, String::from("!"), 2),
//             Token::new_valueless(TokenType::In, String::from("in"), 2),
//             Token::new_valueless(TokenType::Identifier, String::from("france"), 2),
//             Token::new_valueless(TokenType::SelfLower, String::from("self"), 3),
//             Token::new_valueless(TokenType::As, String::from("as"), 3),
//             Token::new_valueless(TokenType::SelfUpper, String::from("Self"), 3),
//             Token::new_valueless(TokenType::Identifier, String::from("be"), 3),
//             Token::new_valueless(TokenType::Identifier, String::from("your"), 3),
//             Token::new_literal(
//                 TokenType::True,
//                 String::from("true"),
//                 3,
//                 LiteralValue::Bool(true),
//             ),
//             Token::new_valueless(TokenType::SelfLower, String::from("self"), 3),
//             Token::new_valueless(TokenType::FloatType, String::from("float"), 4),
//             Token::new_valueless(TokenType::In, String::from("in"), 4),
//             Token::new_valueless(TokenType::IntType, String::from("int"), 4),
//             Token::new_valueless(TokenType::Identifier, String::from("take"), 5),
//             Token::new_valueless(TokenType::Identifier, String::from("your"), 5),
//             Token::new_valueless(TokenType::Identifier, String::from("time"), 5),
//             Token::new_valueless(TokenType::Identifier, String::from("eof"), 13),
//             Token::new_valueless(TokenType::EOF, String::from(""), 13),
//         ];

//         let result = lexer.get_tokens();
//         for i in 0..expected.len() - 1 {
//             if expected[i] != result[i] {
//                 dbg!(&expected[i]);
//                 dbg!(&result[i]);
//                 break;
//             }
//         }

//         assert_eq!(*lexer.get_tokens(), expected);
//     }

//     #[test]
//     fn test_numerics() {
//         let source = "99 -6543 9 0 -1234567
//         88.8 -106.12 // The best Pokemon games are probably HG/SS but anything gen 3-5 is a valid choice
//         34291.123456 -0.15325 100000000.3333 2345654.346765 -2345654.346765 8. -9. -0 -0.";

//         let mut lexer = Lexer::new(source);
//         let expected = vec![
//             Token::new_literal(
//                 TokenType::IntLiteral,
//                 String::from("99"),
//                 1,
//                 LiteralValue::Int(99),
//             ),
//             Token::new_literal(
//                 TokenType::IntLiteral,
//                 String::from("-6543"),
//                 1,
//                 LiteralValue::Int(-6543),
//             ),
//             Token::new_literal(
//                 TokenType::IntLiteral,
//                 String::from("9"),
//                 1,
//                 LiteralValue::Int(9),
//             ),
//             Token::new_literal(
//                 TokenType::IntLiteral,
//                 String::from("0"),
//                 1,
//                 LiteralValue::Int(0),
//             ),
//             Token::new_literal(
//                 TokenType::IntLiteral,
//                 String::from("-1234567"),
//                 1,
//                 LiteralValue::Int(-1234567),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("88.8"),
//                 2,
//                 LiteralValue::Float(88.8),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("-106.12"),
//                 2,
//                 LiteralValue::Float(-106.12),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("34291.123456"),
//                 3,
//                 LiteralValue::Float(34291.123456),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("-0.15325"),
//                 3,
//                 LiteralValue::Float(-0.15325),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("100000000.3333"),
//                 3,
//                 LiteralValue::Float(100000000.3333),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("2345654.346765"),
//                 3,
//                 LiteralValue::Float(2345654.346765),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("-2345654.346765"),
//                 3,
//                 LiteralValue::Float(-2345654.346765),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("8."),
//                 3,
//                 LiteralValue::Float(8.),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("-9."),
//                 3,
//                 LiteralValue::Float(-9.),
//             ),
//             Token::new_literal(
//                 TokenType::IntLiteral,
//                 String::from("-0"),
//                 3,
//                 LiteralValue::Int(-0),
//             ),
//             Token::new_literal(
//                 TokenType::FloatLiteral,
//                 String::from("-0."),
//                 3,
//                 LiteralValue::Float(-0.),
//             ),
//             Token::new_valueless(TokenType::EOF, String::from(""), 3),
//         ];

//         assert_eq!(*lexer.get_tokens(), expected);
//     }
// }
