use crate::ast;
use crate::ast::{
    ASTNode, BinaryOperation, ComparisonOp, MathExpression, MathOperand, MathOperator, Scalar,
};
use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use pest_derive::Parser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        // Precedence is defined lowest to highest
        // | indicates equal precedence
        PrattParser::new()
            .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
            .op(Op::infix(Rule::mul, Left) | Op::infix(Rule::div, Left))
    };
}

#[derive(Parser)]
#[grammar = "ldpl.pest"]
pub struct LDPLParser;

pub(crate) fn parse_expression(pair: pest::iterators::Pair<Rule>) -> ASTNode {
    match pair.as_rule() {
        Rule::number => ASTNode::Scalar(Scalar::Number(pair.as_str().parse().unwrap())),
        Rule::string => ASTNode::Scalar(Scalar::Text(pair.as_str().to_string())),
        Rule::identifier => ASTNode::Identifier(pair.as_str().to_string()),
        Rule::comparison => {
            let mut inner_pairs = pair.into_inner();
            let left = Box::new(parse_expression(inner_pairs.next().unwrap()));
            let operator = inner_pairs.next().unwrap();
            let op = match operator.as_rule() {
                Rule::clte => ComparisonOp::LessThanOrEqual,
                Rule::cgte => ComparisonOp::GreaterThanOrEqual,
                Rule::ceq => ComparisonOp::Equal,
                Rule::cneq => ComparisonOp::NotEqual,
                Rule::clt => ComparisonOp::LessThan,
                Rule::cgt => ComparisonOp::GreaterThan,
                _ => unreachable!("Unknown comparison operator: {:?}", operator.as_str()),
            };
            let right = Box::new(parse_expression(inner_pairs.next().unwrap()));
            ASTNode::Comparison(ast::Comparison { left, op, right })
        }
        Rule::math_expression => ASTNode::MathExpression(parse_math_expression(pair.into_inner())),
        Rule::display => {
            let mut inner_pairs = pair.into_inner();
            let mut nodes = Vec::new();
            while let Some(inner_pair) = inner_pairs.next() {
                nodes.push(parse_expression(inner_pair));
            }
            ASTNode::Display(ast::Display { nodes })
        }
        Rule::if_statement => {
            let mut inner_pairs = pair.into_inner();
            let guard = Box::new(parse_expression(inner_pairs.next().unwrap()));
            let block = parse_block(inner_pairs.next().unwrap());
            let else_if_statements = inner_pairs
                .next()
                .unwrap()
                .into_inner()
                .map(|pair| {
                    let mut inner_pairs = pair.into_inner();
                    let guard = Box::new(parse_expression(inner_pairs.next().unwrap()));
                    let block = parse_block(inner_pairs.next().unwrap());
                    ASTNode::ElseIfStatement(ast::ElseIfStatement { guard, block })
                })
                .collect();
            let else_statement = inner_pairs.next().map(|pair| {
                let block = pair.into_inner().map(parse_expression).collect();
                Box::new(ASTNode::ElseStatement(ast::ElseStatement { block }))
            });
            ASTNode::IfStatement(ast::IfStatement {
                guard,
                block,
                else_if_statements,
                else_statement,
            })
        }
        Rule::while_statement => {
            let mut inner_pairs = pair.into_inner();
            let guard = Box::new(parse_expression(inner_pairs.next().unwrap()));
            let block = parse_block(inner_pairs.next().unwrap());
            ASTNode::WhileStatement(ast::WhileStatement { guard, block })
        }
        Rule::store => {
            let mut inner_pairs = pair.into_inner();
            let value = Box::new(parse_expression(inner_pairs.next().unwrap()));
            let identifier = inner_pairs.next().unwrap().as_str().to_string();
            ASTNode::Store(ast::Store { value, identifier })
        }
        Rule::variable_declaration => {
            let mut inner_pairs = pair.into_inner();
            let identifier = inner_pairs.next().unwrap().as_str().to_string();
            let variable_type = match inner_pairs.next().unwrap().as_str() {
                "number" => ast::VariableType::Number,
                "text" => ast::VariableType::Text,
                _ => unreachable!(),
            };
            ASTNode::VariableDeclaration(ast::VariableDeclaration {
                identifier,
                variable_type,
            })
        }
        Rule::solve => {
            let mut inner_pairs = pair.into_inner();
            let identifier = inner_pairs.next().unwrap().as_str().to_string();
            let math_expression = parse_expression(inner_pairs.next().unwrap());
            match math_expression {
                ASTNode::MathExpression(math_expression) => ASTNode::Solve(ast::Solve {
                    identifier,
                    math_expression,
                }),
                _ => panic!("Expected math expression in solve statement"),
            }
        }
        Rule::guard => {
            let mut inner_pairs = pair.into_inner();
            parse_expression(inner_pairs.next().unwrap())
        }
        Rule::EOI => ASTNode::EOI,
        _ => panic!("Unknown expression type: {:?}", pair.as_rule(),),
    }
}

fn parse_block(pair: pest::iterators::Pair<Rule>) -> Vec<ASTNode> {
    pair.into_inner().map(parse_expression).collect()
}

fn parse_math_expression(pairs: Pairs<Rule>) -> MathExpression {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => MathExpression::Operand(MathOperand::Number(
                primary.as_str().parse::<f32>().unwrap(),
            )),
            Rule::identifier => {
                MathExpression::Operand(MathOperand::Identifier(primary.as_str().to_string()))
            }
            Rule::math_expression => parse_math_expression(primary.into_inner()),
            other => unreachable!("Expr::parse expected math_atom, found {:?}", other),
        })
        .map_infix(|lhs, op, rhs| {
            let operator = match op.as_rule() {
                Rule::add => MathOperator::Add,
                Rule::sub => MathOperator::Subtract,
                Rule::mul => MathOperator::Multiply,
                Rule::div => MathOperator::Divide,
                other => unreachable!("Expr::parse expected infix operation, found {:?}", other),
            };
            MathExpression::BinaryOperation(BinaryOperation {
                left: Box::new(lhs),
                operator,
                right: Box::new(rhs),
            })
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::unary_minus => MathExpression::UnaryMinus(Box::new(rhs)),
            other => unreachable!("Expr::parse expected prefix operation, found {:?}", other),
        })
        .parse(pairs)
}
