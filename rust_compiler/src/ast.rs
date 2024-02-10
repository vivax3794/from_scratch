#[derive(Debug)]
pub struct File(pub Box<[Declaration]>);

#[derive(Debug)]
pub enum Declaration {
    Function(FunctionDeclration),
}

#[derive(Debug)]
pub enum FunctionDeclration {
    ExposedFunction {
        name: Ident,
        return_type: Type,
        body: Body,
    },
}

#[derive(Debug)]
pub enum Type {
    Named(Ident),
    Range(i128, i128),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident(pub Box<str>);

#[derive(Debug)]
pub struct Body(pub Box<[Statement]>);

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Assert(Expression),
    VaribleBinding {
        name: Ident,
        type_: Type,
        value: Expression,
        mutable: bool,
    },
    Assign {
        name: Ident,
        op: Option<BinaryOp>,
        expr: Expression,
    },
    If {
        condition: Expression,
        body: Body,
        elif: Box<[(Expression, Body)]>,
        else_block: Option<Body>,
    },
    WhileLoop {
        condition: Expression,
        body: Body,
    },
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Prefix(PrefixOp, Box<Expression>),
    Comparison(Box<Expression>, Vec<(Comparisson, Box<Expression>)>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Identifier(Ident),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    FloorDivision,
    Mod,
}

#[derive(Debug, Clone, Copy)]
pub enum Comparisson {
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Ne,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum Literal {
    Int(i128),
    Bool(bool),
}
