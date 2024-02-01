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
}

#[derive(Debug)]
pub struct Ident(pub Box<str>);

#[derive(Debug)]
pub struct Body(pub Box<[Statement]>);

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Assert(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Prefix(PrefixOp, Box<Expression>),
    Comparison(Box<Expression>, Vec<(Comparisson, Box<Expression>)>),
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
}

#[derive(Debug)]
pub enum Literal {
    Int(i128),
    Bool(bool),
}
