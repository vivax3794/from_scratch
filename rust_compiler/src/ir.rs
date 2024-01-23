#[derive(Debug)]
pub struct File(pub Box<[Declaration]>);

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: FunctionName,
        return_type: Type,
        body: Body,
    },
}

#[derive(Debug)]
pub enum FunctionName {
    Mangled(Identifier),
    Named(Box<str>),
}

#[derive(Debug)]
pub enum Type {
    Int(usize),
}

#[derive(Debug)]
pub struct Body(pub Box<[Statement]>);

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    LiteralInt { value: isize, width: usize },
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier(pub usize);

impl From<FunctionName> for Box<str> {
    fn from(value: FunctionName) -> Self {
        match value {
            FunctionName::Named(name) => name,
            FunctionName::Mangled(id) => id.into(),
        }
    }
}

impl From<Identifier> for Box<str> {
    fn from(value: Identifier) -> Self {
        format!("Mangled_{:x}", value.0).into_boxed_str()
    }
}
