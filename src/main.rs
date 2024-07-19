use pest::iterators::Pair;
use pest_derive::Parser;
use std::collections::HashMap;
use std::env;
use std::fs;
use pest::Parser;

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

struct Interpreter<'a> {
    variable_values: HashMap<&'a str, VariableValue>,
    variable_types: HashMap<&'a str, VariableType>,
}

impl<'a> Interpreter<'a> {
    fn new() -> Interpreter<'a> {
        Interpreter {
            variable_values: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    fn handle_value(&self, pair: Pair<Rule>) -> VariableValue {
        match pair.as_rule() {
            Rule::number => VariableValue::Number(pair.as_str().parse().unwrap()),
            Rule::string => VariableValue::Text(pair.as_str().to_string()),
            Rule::identifier => {
                let variable_name = pair.as_str();
                let variable_value = self.variable_values
                    .get(variable_name)
                    .expect(&format!("variable {} not found", variable_name));
                variable_value.clone()
            }
            _ => panic!("Unknown variable type"),
        }
    }

    fn handle_comparison(
        &self,
        comparison: Pair<Rule>,
    ) -> bool {
        let mut inner_rules = comparison.into_inner();
        let left = inner_rules.next().unwrap();
        let op = inner_rules.next().unwrap();
        let right = inner_rules.next().unwrap();

        let left_val = self.handle_value(left);
        let right_val = self.handle_value(right);

        match op.as_str() {
            "is equal to" => match (left_val, right_val) {
                (VariableValue::Number(left), VariableValue::Number(right)) => left == right,
                _ => panic!("Cannot compare different types"),
            },
            "is not equal to" => match (left_val, right_val) {
                (VariableValue::Number(left), VariableValue::Number(right)) => left != right,
                _ => panic!("Cannot compare different types"),
            },
            "is greater than" => match (left_val, right_val) {
                (VariableValue::Number(left), VariableValue::Number(right)) => left > right,
                _ => panic!("Cannot compare different types"),
            },
            "is less than" => match (left_val, right_val) {
                (VariableValue::Number(left), VariableValue::Number(right)) => left < right,
                _ => panic!("Cannot compare different types"),
            },
            "is greater than or equal to" => match (left_val, right_val) {
                (VariableValue::Number(left), VariableValue::Number(right)) => left >= right,
                _ => panic!("Cannot compare different types"),
            },
            "is less than or equal to" => match (left_val, right_val) {
                (VariableValue::Number(left), VariableValue::Number(right)) => left <= right,
                _ => panic!("Cannot compare different types"),
            },
            _ => panic!("Unhandled comparison for now"),
        }
    }

    fn handle_guard(&mut self, guard: Pair<Rule>) -> bool {
        let mut inner_rules = guard.into_inner();
        let comparison_rules = inner_rules.next().unwrap();
        self.handle_comparison(comparison_rules)
    }

    fn handle_display(&self, display_pair: Pair<Rule>) {
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
                    let variable_value = self.variable_values
                        .get(variable_name)
                        .expect(&format!("variable {} not found", variable_name));
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

    fn handle_block(&mut self, block: Pair<'a, Rule>) {
        for pair in block.into_inner() {
            self.handle_pair(pair);
        }
    }

    fn handle_if_statement(&mut self, if_statement: Pair<'a, Rule>) {
        let mut inner_rules = if_statement.into_inner();
        let guard = inner_rules.next().unwrap();
        let guard_result = self.handle_guard(guard);
        if guard_result {
            let if_block = inner_rules.next().unwrap();
            self.handle_block(if_block);
            return;
        }
        // Discard the positive if branch
        let _ = inner_rules.next();
        // Loop over each else-if/else
        while let Some(pair) = inner_rules.next() {
            match pair.as_rule() {
                Rule::else_if_statement => {
                    let mut inner_rules = pair.into_inner();
                    let else_guard = inner_rules.next().unwrap();
                    let guard_result = self.handle_guard(else_guard);
                    if guard_result {
                        let else_if_block = inner_rules.next().unwrap();
                        self.handle_block(else_if_block);
                        break;
                    }
                }
                Rule::else_statement => {
                    let mut else_inner = pair.into_inner();
                    let else_block = else_inner.next().unwrap();
                    self.handle_block(else_block);
                    break;
                }
                _ => panic!("Unknown expression type: {:?}", pair.as_rule(), ),
            }
        }
    }

    fn handle_pair(
        &mut self,
        pair: Pair<'a, Rule>,
    ) {
        match pair.as_rule() {
            Rule::display => {
                self.handle_display(pair);
            }
            Rule::if_statement => {
                self.handle_if_statement(pair);
            }
            Rule::store => {
                let mut inner_rules = pair.into_inner().clone();
                let variable_value = inner_rules.next().unwrap();
                let variable_value = match variable_value.as_rule() {
                    Rule::number => VariableValue::Number(variable_value.as_str().parse().unwrap()),
                    Rule::string => VariableValue::Text(variable_value.as_str().to_string()),
                    _ => panic!("Unknown variable type"),
                };
                let variable_name = inner_rules.next().unwrap().as_str();
                self.variable_values.insert(variable_name, variable_value);
            }
            Rule::variable_declaration => {
                let mut inner_rules = pair.into_inner().clone();
                let variable_name = inner_rules.next().unwrap().as_str();
                let variable_type = inner_rules.next().unwrap().as_str();
                let variable_type = match variable_type {
                    "number" => VariableType::Number,
                    "text" => VariableType::Text,
                    _ => panic!("Unknown variable type"),
                };
                self.variable_types.insert(variable_name, variable_type);
            }
            Rule::EOI => {}
            _ => panic!("Unknown expression type: {:?}", pair.as_rule(), ),
        }
    }
}

fn main() {
    let mut interpreter = Interpreter::new();

    let path = env::current_dir().unwrap();
    println!("The current directory is {}", path.display());
    let unparsed_file =
        fs::read_to_string("test.ldpl").expect("Should have been able to read the file");
    let expressions = LDPLParser::parse(Rule::file, &unparsed_file).expect("unsuccessful parse");
    for exp in expressions {
        interpreter.handle_pair(exp);
    }
}
