//! Error handling for the compiler
use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, lexer};

/// Test fail reasons
#[derive(Debug, Error, Diagnostic)]
pub enum TestError {
    /// Error running the test binary
    #[error("Error running the test binary")]
    TestBinaryError,
    /// Error compiling the test binary
    #[error("Error compiling the test binary")]
    TestCompileError {
        /// The error
        #[diagnostic_source]
        error: miette::Report,
    },
    /// Xfail test compiled successfully
    #[error("Xfail test compiled successfully")]
    TestXfail,
    /// Test binary took too long to run
    #[error("Test binary took too long to run")]
    TestTimeout,
}

/// Possible errors that can come from the compiler.
#[derive(Debug, Error, Diagnostic)]
#[diagnostic()]
pub enum Error {
    /// Test failrue error
    #[error("Test failed")]
    TestFailure {
        /// The errors for each of the failed tests
        #[related]
        fails: Vec<TestError>,
    },
    /// A io erorr
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    /// A Presist file error
    #[error("Persist file error: {0}")]
    PersistFile(#[from] tempfile::PersistError),
    /// Subprocess error
    #[error("Subprocess error: {0}")]
    Subprocess(#[from] std::process::ExitStatusError),
    /// Non utf8 path error
    #[error("Non utf8 path error")]
    NonUtf8Path,
    /// Lexer error for a unknown character
    #[error("Unknown character: {character}")]
    UnknownCharacter {
        /// The character that is unknown
        character: char,
        /// The span of the character
        #[label]
        span: miette::SourceSpan,
    },
    /// Unexpected token error in parser
    #[error("Unexpected token: {token:?}, expected: {expected}")]
    UnexpectedToken {
        /// The token that was unexpected
        token: lexer::Token,
        /// Msg for what was expected
        expected: String,
        /// The span of the token
        #[label]
        span: miette::SourceSpan,
    },
    /// Type mismatch error
    #[error("Type mismatch: {expected} != {actual}")]
    TypeMismatch {
        /// The expected type
        expected: String,
        /// The actual type
        actual: String,
        /// The span of the type mismatch
        #[label("Got {actual}")]
        span: miette::SourceSpan,
        /// The reason this type was expected
        #[label("Expected {expected} because of this")]
        reason: Option<miette::SourceSpan>,
    },
    /// Invalid range error
    #[error("Invalid range: {start}..{end} can not be repersented by any width integer")]
    TooLargeRange {
        /// The start of the range
        start: i128,
        /// The end of the range
        end: i128,
        /// The span of the range
        #[label]
        span: miette::SourceSpan,
    },
    /// Invalid range error
    #[error("Invalid range: {start}..{end} is not a valid range")]
    InvalidRange {
        /// The start of the range
        start: i128,
        /// The end of the range
        end: i128,
        /// The span of the range
        #[label]
        span: miette::SourceSpan,
    },
    /// Cant mutate immutable variable
    #[error("Can't mutate immutable variable")]
    CantMutateImmutable {
        /// The span of the mutation
        #[label]
        span: miette::SourceSpan,
        /// The span of the variable declaration
        #[label("Variable declared here")]
        decl_span: miette::SourceSpan,
    },
    /// Invalid assignment target
    #[error("Invalid assignment target, {reason}")]
    InvalidAssignmentTarget {
        /// The reason the assignment is invalid
        reason: String,
        /// The span of the invalid assignment
        #[label]
        span: miette::SourceSpan,
    },
    /// Variable not found error
    #[error("Variable not defined")]
    VariableNotFound {
        /// The span of the variable
        #[label]
        span: miette::SourceSpan,
    },
    /// Division range contains zero    
    #[error("Potential division by zero, because the range {range} contains zero")]
    DivisionByZero {
        /// The span of the divident
        #[label]
        span: miette::SourceSpan,
        /// The range of the divident
        range: String,
    },
    /// Invalid binary operation for this type
    #[error("Invalid binary operation")]
    InvalidBinaryOperation {
        /// The operation that is invalid
        op: ast::BinaryOp,
        /// The type of the operation
        type_: String,
        /// The span of the operation
        #[label("Invalid operation for type {type_}")]
        op_span: miette::SourceSpan,
    },
    /// Invalid right hand side for pow
    #[error("Right hand side of pow cant be negative, which {min}..{max} might be")]
    InvalidPowRhs {
        /// The min of the range
        min: i128,
        /// The max of the range
        max: i128,
        /// The span of the right hand side
        #[label]
        span: miette::SourceSpan,
    },
}

/// A result type with the error type set to the custom error type
pub type Result<T, E = Error> = std::result::Result<T, E>;
