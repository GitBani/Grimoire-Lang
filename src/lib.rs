// utilities
pub mod errors;

// compiler phases
pub mod lexer;

// types / data structures
pub mod token;

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
