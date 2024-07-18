use std::collections::HashMap;
use pest::Parser;
use pest_derive::Parser;
use std::env;
use std::fs;

#[derive(Parser)]
#[grammar = "ldpl.pest"]
pub struct LDPLParser;

#[derive(Debug)]
enum VariableType {
    Number,
    Text,
}

#[derive(Debug, Clone)]
enum VariableValue {
    Number(f32),
    Text(String),
}

fn handle_display(display_pair: pest::iterators::Pair<Rule>, variable_values: &HashMap<&str, VariableValue>) {
    for display_pair in display_pair.into_inner() {
        match display_pair.as_rule() {
            Rule::string => {
                print!("{}", display_pair.as_str());
            }
            Rule::identifier => {
                let variable_name = display_pair.as_str();
                if variable_name == "clrf" {
                    print!("\n");
                    continue;
                }
                let variable_value = variable_values.get(variable_name).
                    expect(&format!("variable {} not found", variable_name));
                match variable_value {
                    VariableValue::Number(value) => {
                        print!("{}", value);
                    }
                    VariableValue::Text(value) => {
                        print!("{}", value);
                    }
                }
            }
            _ => panic!("Unknown display type"),
        }
    }
}

fn main() {
    // map from variable name to type
    let mut variable_types: HashMap<&str, VariableType> = HashMap::new();
    let mut variable_values: HashMap<&str, VariableValue> = HashMap::new();

    let path = env::current_dir().unwrap();
    println!("The current directory is {}", path.display());
    let unparsed_file =
        fs::read_to_string("test.ldpl").expect("Should have been able to read the file");
    let expressions = LDPLParser::parse(Rule::file, &unparsed_file)
        .expect("unsuccessful parse");
    for expression in expressions {
        match expression.as_rule() {
            Rule::display => {
                handle_display(expression, &variable_values);
            },
            Rule::store => {
                let mut inner_rules = expression.into_inner();
                let variable_value = inner_rules.next().unwrap();
                let variable_value = match variable_value.as_rule() {
                    Rule::number => VariableValue::Number(variable_value.as_str().parse().unwrap()),
                    Rule::string => VariableValue::Text(variable_value.as_str().to_string()),
                    _ => panic!("Unknown variable type"),
                };
                let variable_name = inner_rules.next().unwrap().as_str();
                variable_values.insert(variable_name, variable_value);
            }
            Rule::variable_declaration => {
                let mut inner_rules = expression.into_inner();
                let variable_name = inner_rules.next().unwrap().as_str();
                let variable_type = inner_rules.next().unwrap().as_str();
                let variable_type = match variable_type {
                    "number" => VariableType::Number,
                    "text" => VariableType::Text,
                    _ => panic!("Unknown variable type"),
                };
                variable_types.insert(variable_name, variable_type);
            },
            Rule::EOI => {
            }
            _ => panic!(
                "Unknown expression type: {:?}", expression.as_rule(),
            ),
        }
    }
}
