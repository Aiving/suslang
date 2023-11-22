use crate::lexer::token::{Positioned, Token, ValueType};

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Byte(u8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Bool(bool),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

#[derive(PartialEq, Hash, Debug, Eq, Clone)]
pub struct Ident(pub String);

impl Ident {
    pub fn new<T: Into<String>>(string: T) -> Ident {
        Ident(string.into())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionArgument {
    pub name: Positioned<Ident>,
    pub ty: Positioned<ValueType>,
    pub nullable: bool,
    pub default_value: Option<Positioned<Expression>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ClassField {
    pub access_flags: Vec<Positioned<Token>>,
    pub name: Positioned<Ident>,
    pub ty: Option<Positioned<ValueType>>,
    pub nullable: bool,
    pub default_value: Option<Positioned<Expression>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct InterfaceField {
    pub key: Positioned<Ident>,
    pub nullable: bool,
    pub ty: Positioned<ValueType>,
}

pub type Program = Vec<Positioned<Statement>>;

#[derive(PartialEq, Debug, Clone)]
pub struct VariableStatement {
    pub name: Positioned<Ident>,
    pub ty: Option<Positioned<ValueType>>,
    pub nullable: bool,
    pub changeable: Positioned<bool>,
    pub value: Box<Positioned<Expression>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct InterfaceStatement {
    pub name: Positioned<Ident>,
    pub fields: Vec<Positioned<InterfaceField>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ClassStatement {
    pub name: Positioned<Ident>,
    pub generics: Vec<Positioned<Ident>>,
    pub implements: Vec<Positioned<Ident>>,
    pub constructor: Vec<Positioned<Expression>>,
    pub methods: Vec<Positioned<Expression>>,
    pub properties: Vec<Positioned<ClassField>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Variable(VariableStatement),
    Interface(InterfaceStatement),
    Class(ClassStatement),
    Return(Expression),
    Expression(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub struct IfElseConditionExpression {
    pub condition: Box<Positioned<Expression>>,
    pub then: Positioned<Program>,
    pub otherwise: Option<Positioned<Program>>,
}

pub type ArrayExpression = Vec<Positioned<Expression>>;
pub type ObjectExpression = Vec<Positioned<(Positioned<Literal>, Positioned<Expression>)>>;
pub type BlockExpression = Vec<Positioned<Statement>>;

#[derive(PartialEq, Debug, Clone)]
pub struct InfixExpression {
    pub operation: Positioned<Infix>,
    pub left: Box<Positioned<Expression>>,
    pub right: Box<Positioned<Expression>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IndexExpression {
    pub target: Box<Positioned<Expression>>,
    pub index: Box<Positioned<Expression>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionCallExpression {
    pub function: Box<Positioned<Expression>>,
    pub arguments: Vec<Positioned<Expression>>,
    pub lambda: Option<Positioned<Program>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ClassCreationExpression {
    pub class: Positioned<Ident>,
    pub generics: Vec<Positioned<ValueType>>,
    pub arguments: Positioned<Vec<Positioned<Expression>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ConstructorExpression {
    pub access_flags: Vec<Positioned<Token>>,
    pub name: Positioned<Ident>,
    pub arguments: Positioned<Vec<Positioned<FunctionArgument>>>,
    pub body: Positioned<Program>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ClassFunctionExpression {
    pub access_flags: Vec<Positioned<Token>>,
    pub name: Positioned<Ident>,
    pub return_type: Positioned<ValueType>,
    pub arguments: Positioned<Vec<Positioned<FunctionArgument>>>,
    pub body: Positioned<Program>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionExpression {
    pub access_flags: Vec<Positioned<Token>>,
    pub name: Positioned<Ident>,
    pub return_type: Positioned<ValueType>,
    pub arguments: Positioned<Vec<Positioned<FunctionArgument>>>,
    pub body: Positioned<Program>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct LinearFunctionExpression {
    pub arguments: Positioned<Vec<Positioned<Ident>>>,
    pub body: Positioned<Program>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct AssignmentExpression {
    pub variable: Positioned<Vec<Positioned<Ident>>>,
    pub value: Box<Positioned<Expression>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    This,
    Null,
    Ident(Ident),
    Literal(Literal),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Block(BlockExpression),
    Infix(InfixExpression),
    Index(IndexExpression),
    FunctionCall(FunctionCallExpression),
    ClassCreation(ClassCreationExpression),
    IfCondition(IfElseConditionExpression),
    Constructor(ConstructorExpression),
    ClassFunction(ClassFunctionExpression),
    Function(FunctionExpression),
    LinearFunction(LinearFunctionExpression),
    Assignment(AssignmentExpression),
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PCall,
    PIndex,
}
