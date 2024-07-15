use pest::Parser;
use pest_derive::Parser;
use std::env;
use std::fs;

#[derive(Parser)]
#[grammar = "ldpl.pest"]
pub struct CSVParser;

fn main() {
    let path = env::current_dir().unwrap();
    println!("The current directory is {}", path.display());
    let unparsed_file =
        fs::read_to_string("test.ldpl").expect("Should have been able to read the file");
    let file = CSVParser::parse(Rule::file, &unparsed_file)
        .expect("unsuccessful parse") // unwrap the parse result
        .next()
        .unwrap(); // get and unwrap the `file` rule; never fails
    for expression in file.into_inner() {
        match expression.as_rule() {
            Rule::expression => {
                println!("Expression: {}", expression.as_str());
            }
            Rule::EOI => {
                println!("End of input");
            }
            _ => unreachable!(),
        }
    }
}
