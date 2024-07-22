use crate::ast;
use crate::ast::{ASTNode, Comparison, MathExpression, MathOperand, Scalar, VariableType};
use std::collections::HashMap;

pub struct Interpreter {
    variable_values: HashMap<String, Scalar>,
    variable_types: HashMap<String, VariableType>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            variable_values: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    fn identifier_value(&self, identifier: String) -> &Scalar {
        self
            .variable_values
            .get(identifier.as_str())
            .unwrap_or_else(|| panic!("variable {identifier} not found"))
    }

    fn handle_display(&self, display: ast::Display) {
        for node in display.nodes {
            match node {
                ASTNode::Scalar(scalar) => match scalar {
                    Scalar::Number(value) => print!("{value}"),
                    Scalar::Text(value) => print!("{value}"),
                    Scalar::Boolean(value) => print!("{value}"),
                },
                ASTNode::Identifier(identifier) => {
                    if identifier == "crlf" {
                        println!();
                        return;
                    }
                    let variable_value = self
                        .variable_values
                        .get(identifier.as_str())
                        .unwrap_or_else(|| panic!("variable {identifier} not found"));
                    match variable_value {
                        Scalar::Number(value) => print!("{value}"),
                        Scalar::Text(value) => print!("{value}"),
                        Scalar::Boolean(value) => print!("{value}"),
                    }
                }
                _ => {
                    panic!("Unable to display node of type {:?}", node)
                }
            }
        }
    }

    fn handle_if_statement(&mut self, if_statement: ast::IfStatement) {
        let guard_result = self.handle_guard(&if_statement.guard);
        match guard_result {
            Scalar::Boolean(true) => self.handle_block(if_statement.block),
            Scalar::Boolean(false) => {
                for else_if_statement in if_statement.else_if_statements {
                    let ASTNode::ElseIfStatement(else_if_statement) =
                        else_if_statement
                    else {
                        panic!("Expected else if statement")
                    };
                    let guard_result = self.handle_guard(&else_if_statement.guard);
                    match guard_result {
                        Scalar::Boolean(true) => {
                            self.handle_block(else_if_statement.block);
                            return;
                        }
                        _ => continue,
                    }
                }
                if let Some(else_statement) = if_statement.else_statement {
                    let ASTNode::ElseStatement(else_statement) =
                        *else_statement
                    else {
                        panic!("Expected else statement")
                    };
                    self.handle_block(else_statement.block);
                }
            }
            _ => panic!("Expected boolean"),
        }
    }

    fn handle_guard(&mut self, guard: &Box<ASTNode>) -> Scalar {
        match &**guard {
            ASTNode::Comparison(comparison) => self.handle_comparison(comparison.clone()),
            _ => panic!("Expected comparison"),
        }
    }

    fn handle_comparison_operand(&mut self, operand: ASTNode) -> Scalar {
        match operand {
            ASTNode::Scalar(scalar) => scalar,
            ASTNode::Identifier(identifier) => self.identifier_value(identifier).clone(),
            ASTNode::Comparison(comparison) => self.handle_comparison(comparison),
            _ => panic!("Expected scalar or identifier"),
        }
    }

    fn handle_comparison(&mut self, comparison: Comparison) -> Scalar {
        let left = self.handle_comparison_operand(*comparison.left);
        let right = self.handle_comparison_operand(*comparison.right);
        match comparison.op {
            ast::ComparisonOp::Equal => Scalar::Boolean(left == right),
            ast::ComparisonOp::NotEqual => Scalar::Boolean(left != right),
            ast::ComparisonOp::GreaterThan => Scalar::Boolean(left > right),
            ast::ComparisonOp::LessThan => Scalar::Boolean(left < right),
            ast::ComparisonOp::GreaterThanOrEqual => Scalar::Boolean(left >= right),
            ast::ComparisonOp::LessThanOrEqual => Scalar::Boolean(left <= right),
        }
    }

    fn handle_store(&mut self, store: ast::Store) {
        let var_type = {
            let var_type = self
                .variable_types
                .get(&*store.identifier)
                .expect("Variable not declared");
            var_type.clone()
        };
        let value = match self.handle_node(*store.value).expect("Value not found") {
            ASTNode::Scalar(Scalar::Number(value)) => {
                if let VariableType::Text = var_type.clone() {
                    panic!("Cannot assign number to text");
                }
                if let VariableType::Boolean = var_type {
                    panic!("Cannot assign number to text");
                }
                Scalar::Number(value)
            }
            ASTNode::Scalar(Scalar::Text(value)) => {
                if let VariableType::Number = var_type {
                    panic!("Cannot assign number to text");
                }
                if let VariableType::Boolean = var_type {
                    panic!("Cannot assign number to text");
                }
                Scalar::Text(value)
            }
            _ => panic!("Unable to assign type to variable"),
        };
        self.variable_values.insert(store.identifier, value);
    }

    fn handle_node(&mut self, node: ASTNode) -> Option<ASTNode> {
        match node {
            ASTNode::Scalar(scalar) => match scalar {
                Scalar::Number(value) => Some(ASTNode::Scalar(Scalar::Number(value))),
                Scalar::Text(value) => Some(ASTNode::Scalar(Scalar::Text(value))),
                Scalar::Boolean(value) => Some(ASTNode::Scalar(Scalar::Boolean(value))),
            },
            ASTNode::Identifier(identifier) => {
                let variable_value = self
                    .variable_values
                    .get(identifier.as_str())
                    .unwrap_or_else(|| panic!("variable {identifier} not found"));
                match variable_value {
                    Scalar::Number(value) => Some(ASTNode::Scalar(Scalar::Number(*value))),
                    Scalar::Text(value) => Some(ASTNode::Scalar(Scalar::Text(value.clone()))),
                    Scalar::Boolean(value) => Some(ASTNode::Scalar(Scalar::Boolean(*value))),
                }
            }
            ASTNode::VariableDeclaration(variable_declaration) => {
                // parser already ensures it's either number or text
                self.variable_types.insert(
                    variable_declaration.identifier,
                    variable_declaration.variable_type,
                );
                None
            }
            ASTNode::Store(store) => {
                self.handle_store(store);
                None
            }
            ASTNode::Display(display) => {
                self.handle_display(display);
                None
            }
            ASTNode::Comparison(comparison) => {
                Some(ASTNode::Scalar(self.handle_comparison(comparison)))
            }
            ASTNode::IfStatement(if_statement) => {
                self.handle_if_statement(if_statement);
                None
            }
            ASTNode::WhileStatement(while_statement) => {
                while self.handle_guard(&while_statement.guard) == Scalar::Boolean(true) {
                    self.handle_block(while_statement.block.clone());
                }
                None
            }
            ASTNode::Solve(solve) => {
                let result = self.handle_math_expression(solve.math_expression);
                self.variable_values.insert(solve.identifier, result);
                None
            }
            ASTNode::EOI => None,
            _ => panic!("Unhandled node type: {node:?}"),
        }
    }

    pub fn interpret(&mut self, ast: Vec<ASTNode>) {
        self.handle_block(ast);
    }

    fn handle_block(&mut self, block: Vec<ASTNode>) {
        for node in block {
            self.handle_node(node);
        }
    }

    fn handle_math_expression(&mut self, math_expression: MathExpression) -> Scalar {
        match math_expression {
            MathExpression::Operand(operand) => match operand {
                MathOperand::Number(int) => Scalar::Number(int),
                MathOperand::Identifier(identifier) => {
                    let val = self
                        .variable_values
                        .get(identifier.as_str())
                        .unwrap_or_else(|| panic!("variable {identifier} not found"))
                        .clone();
                    match val {
                        Scalar::Number(_) | Scalar::Boolean(_) => val,
                        _ => panic!("Cannot perform math operations on type {val:?}"),
                    }
                }
            },
            MathExpression::UnaryMinus(operand) => match *operand {
                MathExpression::Operand(MathOperand::Number(value)) => Scalar::Number(-value),
                MathExpression::Operand(MathOperand::Identifier(identifier)) => {
                    let val = self
                        .variable_values
                        .get(identifier.as_str())
                        .unwrap_or_else(|| panic!("variable {identifier} not found"))
                        .clone();
                    match val {
                        Scalar::Number(value) => Scalar::Number(-value),
                        _ => panic!("Cannot perform math operations on type {val:?}"),
                    }
                }
                MathExpression::UnaryMinus(expression) => self.handle_math_expression(*expression),
                MathExpression::BinaryOperation(binop) => {
                    let val = self.handle_math_expression(MathExpression::BinaryOperation(binop));
                    match val {
                        Scalar::Number(value) => Scalar::Number(-value),
                        _ => panic!("Cannot perform math operations on type {val:?}"),
                    }
                }
            },
            MathExpression::BinaryOperation(binop) => {
                let left = self.handle_math_expression(*binop.left);
                let right = self.handle_math_expression(*binop.right);
                match (left, right) {
                    (Scalar::Number(left), Scalar::Number(right)) => match binop.operator {
                        ast::MathOperator::Add => Scalar::Number(left + right),
                        ast::MathOperator::Subtract => Scalar::Number(left - right),
                        ast::MathOperator::Multiply => Scalar::Number(left * right),
                        ast::MathOperator::Divide => Scalar::Number(left / right),
                    },
                    _ => panic!("Binop failed"),
                }
            }
        }
    }
}
