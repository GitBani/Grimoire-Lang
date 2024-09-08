use std::{iter::Peekable, str::Chars};

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    line: usize,
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &String) -> Lexer {
        Lexer {
            source: source.chars().peekable(),
            line: 1,
            offset: 0,
        }
    }

    fn advance(&mut self) {
        self.source.next();
        self.offset += 1;
    }
}
