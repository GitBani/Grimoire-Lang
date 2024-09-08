use std::{env, fs, process};

use grimoire_lang::{lexer::Lexer, Config};

fn main() {
    // read file into string and pass to scanner
    let args: Vec<String> = env::args().collect();
    let config = Config::build(&args).unwrap_or_else(|err| {
        eprintln!("Error parsing arguments: {err}");
        process::exit(1);
    });

    let source = fs::read_to_string(config.path).unwrap_or_else(|err| {
        eprintln!("Error reading from file: {err}");
        process::exit(1);
    });

    let lexer = Lexer::new(&source);
    // return token list from scanner and pass to parser for it to generate wasm
}
