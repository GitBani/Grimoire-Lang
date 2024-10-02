pub fn report_syntax_error(msg: &str, line: usize, col: usize) {
    eprintln!("Syntax error: {msg} at line: {line}, column: {col}");
}
