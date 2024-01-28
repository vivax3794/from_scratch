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

#[derive(Debug, Clone)]
pub enum FunctionName {
    Mangled(Identifier),
    Named(Box<str>),
}

#[derive(Debug)]
pub enum Type {
    Int(usize),
    Bool,
}

#[derive(Debug)]
pub struct Body(pub Box<[Statement]>);

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Assert(BoolExpression),
}

#[derive(Debug)]
pub enum Expression {
    Int(IntExpression),
    Bool(BoolExpression),
}

#[derive(Debug)]
pub enum IntExpression {
    Literal { value: isize, width: usize },
}

#[derive(Debug)]
pub enum BoolExpression {
    Literal(bool),
    Not(Box<BoolExpression>),
    Comparison(IntExpression, Box<[(ComparissonOp, IntExpression)]>),
}

#[derive(Debug)]
pub enum ComparissonOp {
    Eq,
    Ne,
    Gts,
    Lts,
    Ges,
    Les,
    Gtu,
    Ltu,
    Geu,
    Leu,
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier(pub usize);

impl FunctionName {
    pub fn str(&self) -> Box<str> {
        self.clone().into()
    }
}

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
