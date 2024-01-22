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
        return_type: VToCType,
        body: Body,
    },
}

#[derive(Debug)]
pub struct VToCType {
    pub viv_type: Type,
    pub c_type: CType,
}

#[derive(Debug)]
pub enum Type {
    Named(Ident),
}

#[derive(Debug)]
pub enum CType {
    NamedInt(u8),
}

#[derive(Debug)]
pub struct Ident(pub Box<str>);

#[derive(Debug)]
pub struct Body(pub Box<[Statement]>);

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
}

#[derive(Debug)]
pub enum Literal {
    Int(isize),
}
