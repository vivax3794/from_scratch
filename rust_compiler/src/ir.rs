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
    Int(u8),
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
    Literal {
        value: u64,
        width: u8,
    },
    Extend {
        value: Box<IntExpression>,
        target: u8,
        signed: bool,
    },
    Truncate {
        value: Box<IntExpression>,
        target: u8,
    },
    Neg(Box<IntExpression>),
    Binary(Box<IntExpression>, IntBinaryOp, Box<IntExpression>, bool),
}

#[derive(Debug)]
pub enum IntBinaryOp {
    Add,
    Sub,
    Mul,
    FloorDivision,
    Remainder,
}

#[derive(Debug)]
pub enum BoolExpression {
    Literal(bool),
    Not(Box<BoolExpression>),
    Comparison(
        IntExpression,
        Box<[(inkwell::IntPredicate, IntExpression)]>,
    ),
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
