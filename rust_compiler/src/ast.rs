//! This module contains the AST for the language.
//! This represents the structure of the syntax.

use crate::span::{Span, Spanned};

/// The root of the AST
#[derive(Debug)]
pub struct File(pub Box<[Declaration]>);

/// A top level statement
#[derive(Debug)]
pub enum Declaration {
    /// A function declration
    Function(FunctionDeclration),
}

/// A function declration
#[derive(Debug)]
pub enum FunctionDeclration {
    /// A function that does not have its name mangled
    /// and can be called from other languages
    Function {
        /// Should the function be mangeld
        mangled: bool,
        /// Non mangled name
        name: Ident,
        /// The arguments to the function.
        arguments: Box<[Argument]>,
        /// The return type of the function
        return_type: Spanned<Type>,
        /// The body of the function
        body: Body,
    },
}

/// A function argument
#[derive(Debug)]
pub struct Argument {
    /// Is it mutable
    pub mutable: bool,
    /// The name of the argument
    pub name: Spanned<Ident>,
    /// The type of the argument
    pub type_: Spanned<Type>,
}

/// A type
#[derive(Debug, Clone)]
pub enum Type {
    /// A name of a type, i.e struct, an alias, etc
    Named(Ident),
    /// Integer type, which is a minimum and maximum value
    Range(i128, i128),
}

/// An identifier
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident(pub Box<str>);

/// A block of statements
#[derive(Debug)]
pub struct Body(pub Box<[Statement]>);

/// A statement
#[derive(Debug)]
pub enum Statement {
    /// A expression
    Expr(Spanned<Expression>),
    /// A return statement
    Return(Spanned<Expression>),
    /// An assert statement
    /// This will panic if the expression is false
    Assert(Spanned<Expression>, Span),
    /// A type assertion
    AssertType(Spanned<Type>, Spanned<Expression>),
    /// A variable binding
    VaribleBinding {
        /// The name of the variable
        name: Spanned<Ident>,
        /// The type of the variable
        type_: Spanned<Type>,
        /// The inital value of the variable
        value: Spanned<Expression>,
        /// If the variable is mutable
        mutable: bool,
    },
    /// A variable assignment
    Assign {
        /// The target of the assignment
        /// currently only supports identifiers
        target: Spanned<Expression>,
        /// Potential binary operation for compound assignment (e.g. +=, -=, *=, /=, %=, etc.)
        op: Option<Spanned<BinaryOp>>,
        /// The value to assign / the right hand side of the operation
        expr: Spanned<Expression>,
    },
    /// A if statement
    If {
        /// The condition of the if statement
        condition: Spanned<Expression>,
        /// The body of the if statement
        body: Body,
        /// The else if blocks
        elif: Box<[(Spanned<Expression>, Body)]>,
        /// The else block
        else_block: Option<Body>,
    },
    /// A while loop
    WhileLoop {
        /// The condition of the while loop
        condition: Spanned<Expression>,
        /// The body of the while loop
        body: Body,
    },
}

/// An expression
#[derive(Debug, Clone)]
pub enum Expression {
    /// A literal
    Literal(Literal),
    /// A prefix operation
    Prefix(PrefixOp, Box<Spanned<Expression>>),
    /// A comparison chain
    Comparison(
        Box<Spanned<Expression>>,
        Vec<(ComparissonOp, Box<Spanned<Expression>>)>,
    ),
    /// A binary operation
    Binary(Box<Spanned<Expression>>, BinaryOp, Box<Spanned<Expression>>),
    /// A variable
    Identifier(Ident),
    /// A function call
    Call {
        /// The function to call
        function: Box<Spanned<Expression>>,
        /// The arguments to the function
        arguments: Box<[Spanned<Expression>]>,
    },
    /// Type cast
    Cast {
        /// The type to cast to
        type_: Spanned<Type>,
        /// The expression to cast
        expr: Box<Spanned<Expression>>,
    },
}

/// A binary operation
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    FloorDivision,
    /// Modulus
    Mod,
    /// Exponentiation
    Pow,
    /// And
    And,
    /// Or,
    Or,
}

/// A comparison operation
#[derive(Debug, Clone, Copy)]
pub enum ComparissonOp {
    /// Equal
    Eq,
    /// Less than
    Lt,
    /// Greater than
    Gt,
    /// Less than or equal
    Le,
    /// Greater than or equal
    Ge,
    /// Not equal
    Ne,
}

/// A prefix operation
#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    /// Logical not
    Not,
    /// Negate number
    Neg,
}

/// A literal
#[derive(Debug, Clone)]
pub enum Literal {
    /// A integer literal
    Int(i128),
    /// A boolean literal
    Bool(bool),
}
