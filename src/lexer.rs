use std::{iter::Peekable, str::Chars};

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &String) -> Lexer {
        Lexer {
            source: source.chars().peekable(),
            position: 0,
        }
    }
}
