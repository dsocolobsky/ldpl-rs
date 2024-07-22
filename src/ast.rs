#[derive(Debug, Clone)]
pub enum ASTNode {
    Scalar(Scalar),
    Identifier(String),
    VariableDeclaration(VariableDeclaration),
    Store(Store),
    Display(Display),
    Comparison(Comparison),
    IfStatement(IfStatement),
    ElseIfStatement(ElseIfStatement),
    ElseStatement(ElseStatement),
    WhileStatement(WhileStatement),
    MathExpression(MathExpression),
    Solve(Solve),
    EOI,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Scalar {
    Number(f32),
    Text(String),
    Boolean(bool),
}

#[derive(Debug, Clone)]
pub(crate) struct VariableDeclaration {
    pub(crate) identifier: String,
    pub(crate) variable_type: VariableType,
}

#[derive(Debug, Clone)]
pub(crate) struct Store {
    pub(crate) value: Box<ASTNode>,
    pub(crate) identifier: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Display {
    pub(crate) nodes: Vec<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct Comparison {
    pub(crate) left: Box<ASTNode>,
    pub(crate) op: ComparisonOp,
    pub(crate) right: Box<ASTNode>,
}

#[derive(Debug, Clone)]
pub(crate) struct IfStatement {
    pub(crate) guard: Box<ASTNode>,
    pub(crate) block: Vec<ASTNode>,
    pub(crate) else_if_statements: Vec<ASTNode>,
    pub(crate) else_statement: Option<Box<ASTNode>>,
}
#[derive(Debug, Clone)]
pub(crate) struct ElseIfStatement {
    pub(crate) guard: Box<ASTNode>,
    pub(crate) block: Vec<ASTNode>,
}
#[derive(Debug, Clone)]
pub(crate) struct ElseStatement {
    pub(crate) block: Vec<ASTNode>,
}

#[derive(Debug, Clone)]
pub(crate) struct WhileStatement {
    pub(crate) guard: Box<ASTNode>,
    pub(crate) block: Vec<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum MathExpression {
    Operand(MathOperand),
    UnaryMinus(Box<MathExpression>),
    BinaryOperation(BinaryOperation),
}
#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub(crate) left: Box<MathExpression>,
    pub(crate) operator: MathOperator,
    pub(crate) right: Box<MathExpression>,
}
#[derive(Debug, Clone)]
pub enum MathOperand {
    Identifier(String),
    Number(f32),
}
#[derive(Debug, Clone)]
pub(crate) struct Solve {
    pub(crate) identifier: String,
    pub(crate) math_expression: MathExpression,
}

#[derive(Debug, Clone)]
pub enum VariableType {
    Number,
    Text,
    Boolean,
}

#[derive(Debug, Clone)]
pub enum ComparisonOp {
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone)]
pub enum MathOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}
