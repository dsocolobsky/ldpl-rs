mod ast;
mod interpreter;
mod parser;

use crate::parser::{LDPLParser, Rule};
use pest::Parser;
use std::env;
use std::fs;

fn main() {
    let mut interpreter = interpreter::Interpreter::new();

    let path = env::current_dir().unwrap();
    println!("The current directory is {}", path.display());
    let unparsed_file =
        fs::read_to_string("test.ldpl").expect("Should have been able to read the file");
    let expressions = LDPLParser::parse(Rule::file, &unparsed_file).expect("unsuccessful parse");
    let expressions = expressions
        .map(|pair| parser::parse_expression(pair))
        .collect();
    interpreter.interpret(expressions);
}
