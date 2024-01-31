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
    Literal {
        value: isize,
        width: usize,
    },
    // These should not fail, these are for implicit conversitions
    // do not use these for user asked for conversions that might fail (i.e overflow)
    // cast a unsigned value to a higher width
    CastWidthUnsigned {
        value: Box<Expression>,
        target: u8,
    },
    // cast a signed value to a higher width.
    CastWidthSigned {
        value: Box<Expression>,
        target: u8,
        current: u8,
    },
    // note: we do not need a unsigned to signed conversion as that can happen implicitly by just
    // using the unsigned value in the signed values place
    // (if the value could go into the negative range because of its range it will be cast to a
    // higher width first, like a u8 would be cast to a i16, but if the range is know to be 0-5 it
    // would just be a i8)
}

#[derive(Debug)]
pub enum BoolExpression {
    Literal(bool),
    Not(Box<BoolExpression>),
    Comparison(IntExpression, Box<[(inkwell::IntPredicate, IntExpression)]>),
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
