//! Intermediate representation of the compiler.
//! This in contrast to the ast represents the code in a way that is easier to generate code from.

/// The root of the IR
#[derive(Debug)]
pub struct File(pub Box<[Declaration]>);

/// A top level statement
#[derive(Debug)]
pub enum Declaration {
    /// A function declration
    Function {
        /// The name of the function
        name: FunctionName,
        /// The return type of the function
        return_type: Type,
        /// The body of the function
        body: Body,
        /// The variables that are in scope
        vars: Box<[(Identifier, Type)]>,
    },
}

/// A function name, can be mangled or raw
#[derive(Debug, Clone)]
pub enum FunctionName {
    /// A mangled name, which is a random unique identifier
    Mangled(Identifier),
    /// A raw name
    Named(Box<str>),
}

/// Low level type
#[derive(Debug, Clone, Copy)]
pub enum Type {
    /// A integer type of a specific width
    Int(u8),
    /// A boolean type
    Bool,
}

/// A block of statements
#[derive(Debug)]
pub struct Body(pub Box<[Statement]>);

/// A statement
#[derive(Debug)]
pub enum Statement {
    /// A return statement
    Return(Expression),
    /// An assert statement
    Assert(BoolExpression),
    /// A variable binding
    Assign {
        /// Target variable
        name: Identifier,
        /// The value
        value: Expression,
    },
    /// A if statement
    If {
        /// The conditions and the bodies
        conditions: Box<[(BoolExpression, Body)]>,
        /// The else block
        else_block: Option<Body>,
    },
    /// A while loop
    WhileLoop {
        /// The condition
        condition: BoolExpression,
        /// The body
        body: Body,
    },
    /// A expression
    Expression(Expression),
}

/// An expression
/// We have enums of different types of expressions to make it easier to generate code from
/// because for example in a assert statement we can only have a boolean expression
#[derive(Debug, Clone)]
pub enum Expression {
    /// A integer expression
    Int(IntExpression),
    /// A boolean expression
    Bool(BoolExpression),
}

/// A integer expression
#[derive(Debug, Clone)]
pub enum IntExpression {
    /// A integer literal
    Literal {
        /// The value of the literal
        value: u64,
        /// The width of the literal
        width: u8,
    },
    /// Extend a integer
    Extend {
        /// The value to extend
        value: Box<IntExpression>,
        /// The target width
        target: u8,
        /// If the value is signed
        signed: bool,
    },
    /// Truncate a integer
    Truncate {
        /// The value to truncate
        value: Box<IntExpression>,
        /// The target width
        target: u8,
    },
    /// A negation
    Neg(Box<IntExpression>),
    /// A binary operation
    Binary {
        /// The left hand side
        left: Box<IntExpression>,
        /// The operation
        op: IntBinaryOp,
        /// The right hand side
        right: Box<IntExpression>,
        /// Is the operation signed
        signed: bool,
        /// The bounds of the result
        /// anything outside will be limited to the bounds
        bounds: Option<(u64, u64)>,
        /// The width of the result
        width: u8,
    },
    /// A variable
    LoadVar(Identifier),
}

/// A integer binary operation
#[derive(Debug, Clone)]
pub enum IntBinaryOp {
    /// A addition
    Add,
    /// A subtraction
    Sub,
    /// A multiplication
    Mul,
    /// A division
    FloorDivision,
    /// A modulus
    Remainder,
}

/// A boolean expression
#[derive(Debug, Clone)]
pub enum BoolExpression {
    /// A boolean literal
    Literal(bool),
    /// A negation
    Not(Box<BoolExpression>),
    /// A comparison chain
    Comparison(
        IntExpression,
        Box<[(inkwell::IntPredicate, IntExpression)]>,
    ),
    /// A variable
    LoadVar(Identifier),
}

/// A identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Identifier(pub uuid::Uuid);

impl Identifier {
    /// Create a new random identifier
    pub fn new() -> Self {
        let id = uuid::Uuid::new_v4();
        Self(id)
    }
}

impl Default for Identifier {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionName {
    /// Get the string representation of the function name
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
