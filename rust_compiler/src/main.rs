//! Rust compiler for my custom language

#![feature(map_try_insert)]
#![feature(exit_status_error)]
#![warn(
    clippy::pedantic,
    clippy::clone_on_ref_ptr,
    clippy::create_dir,
    clippy::filetype_is_file,
    clippy::fn_to_numeric_cast_any,
    clippy::if_then_some_else_none,
    missing_docs,
    clippy::missing_docs_in_private_items,
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::missing_const_for_fn,
    clippy::mixed_read_write_in_expression,
    clippy::partial_pub_fields,
    clippy::same_name_method,
    clippy::str_to_string,
    clippy::suspicious_xor_used_as_pow,
    clippy::try_err,
    clippy::unneeded_field_pattern,
    clippy::use_debug,
    clippy::verbose_file_reads,
    clippy::expect_used,
    clippy::unreachable
)]
#![deny(
    clippy::unwrap_used,
    clippy::panic,
    clippy::unimplemented,
    clippy::todo,
    clippy::dbg_macro,
    clippy::error_impl_error,
    clippy::exit,
    clippy::panic_in_result_fn,
    clippy::tests_outside_test_module
)]
use clap::Parser;
use miette::Diagnostic;
use std::path::{Path, PathBuf};
use thiserror::Error;

mod ast;
mod codegen;
mod ir;
mod lexer;
mod parser;
mod type_system;

/// Test fail reasons
#[derive(Debug, Error, Diagnostic)]
enum TestError {
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
}

/// Possible errors that can come from the compiler.
#[derive(Debug, Error, Diagnostic)]
#[diagnostic()]
enum Error {
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
        /// The span of the expression
        #[label("Expression of type {type_}")]
        expr_span: miette::SourceSpan,
    },
}

/// A result type with the error type set to the custom error type
type Result<T, E = Error> = std::result::Result<T, E>;

/// A span
#[derive(Debug, Clone, Copy)]
struct Span {
    /// The start of the span
    from: usize,
    /// The end of the span
    to: usize,
}

/// A spanned value
#[derive(Debug, Clone, Copy)]
struct Spanned<T> {
    /// The value
    value: T,
    /// The span
    span: Span,
}

impl Span {
    /// Create a new span
    const fn new(from: usize, to: usize) -> Self {
        Self { from, to }
    }
    ///
    /// Combine two spanned values
    const fn combine(&self, other: Span) -> Span {
        Span::new(self.from, other.to)
    }

    /// Map the value of the spanned value
    const fn with_value<R>(&self, value: R) -> Spanned<R> {
        Spanned::new(value, self.from, self.to)
    }
}

impl<T> Spanned<T> {
    /// Create a new spanned value
    const fn new(value: T, from: usize, to: usize) -> Self {
        Self {
            value,
            span: Span { from, to },
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        Self::new(value.from.into(), value.to - value.from)
    }
}
impl From<&Span> for miette::SourceSpan {
    fn from(value: &Span) -> Self {
        Self::new(value.from.into(), value.to - value.from)
    }
}

/// Build the input build and store the resulting binary in the output file
fn build(content: &str, output_file: &Path) -> Result<(), Error> {
    let tokens = lexer::Lexer::new(content).lex()?;
    let ast = parser::Parser::new(tokens).parse()?;

    let mut type_resolver = type_system::TypeResolver::new();
    let ir = type_resolver.resolve_file(&ast)?;

    let ctx = inkwell::context::Context::create();
    let mut gen = codegen::CodeGen::new(&ctx);
    gen.generate_file(&ir);

    let llvm_ir = tempfile::NamedTempFile::new()?;
    let object_file = tempfile::NamedTempFile::new()?;
    gen.save_to_file(llvm_ir.path());
    std::process::Command::new("llc")
        .args([
            llvm_ir.path().to_str().ok_or(Error::NonUtf8Path)?,
            "-filetype=obj",
            "-o",
            object_file.path().to_str().ok_or(Error::NonUtf8Path)?,
        ])
        .status()?
        .exit_ok()?;
    std::process::Command::new("gcc")
        .args([
            object_file.path().to_str().ok_or(Error::NonUtf8Path)?,
            "-no-pie",
            "-o",
            output_file.to_str().ok_or(Error::NonUtf8Path)?,
        ])
        .status()?
        .exit_ok()?;

    Ok(())
}

/// Build the input build and store the resulting binary in the output file
/// this is different from `build` in that it wraps the error in a miette error
fn build_wrapped(
    content: String,
    input_file: &Path,
    output_file: &Path,
) -> Result<(), miette::Report> {
    build(&content, output_file).map_err(|err| {
        let err: miette::Report = err.into();
        err.with_source_code(miette::NamedSource::new(
            #[allow(clippy::expect_used)]
            input_file
                .to_str()
                .ok_or(Error::NonUtf8Path)
                .expect("Would have errored before this"),
            content,
        ))
    })
}

/// Compiler arguments
#[derive(Parser)]
enum Commands {
    /// Build a single file
    Build {
        /// The file to compiler
        input: PathBuf,
        /// The output file
        output: PathBuf,
    },
    /// Compile and run all files in a directory
    Test {
        /// The directory to compile
        directory: PathBuf,
    },
}

fn main() -> miette::Result<()> {
    let commands = Commands::parse();
    match commands {
        Commands::Build { input, output } => {
            let content = std::fs::read_to_string(&input)
                .map_err(Error::from)?;
            build_wrapped(content, &input, &output)?;
        }
        Commands::Test { directory } => {
            let mut failed = Vec::new();
            for entry in
                std::fs::read_dir(directory).map_err(Error::from)?
            {
                let entry = entry.map_err(Error::from)?;
                let path = entry.path();
                if path.extension().is_some_and(|ext| ext == "viv") {
                    let output = tempfile::NamedTempFile::new()
                        .map_err(Error::from)?;
                    let content = std::fs::read_to_string(&path)
                        .map_err(Error::from)?;

                    match build_wrapped(content, &path, output.path())
                    {
                        Ok(()) => {}
                        Err(err) => {
                            println!(
                                "❌ {}",
                                path.file_name()
                                .map(|name| name.to_string_lossy())
                                .unwrap_or_default()
                            );
                            failed.push(
                                TestError::TestCompileError {
                                    error: err,
                                },
                            );
                            continue;
                        }
                    }

                    let (file, output_path) =
                        output.keep().map_err(Error::from)?;
                    drop(file);

                    let status =
                        std::process::Command::new(&output_path)
                            .status()
                            .map_err(Error::from)?;
                    println!(
                        "{} {}",
                        if status.success() { "✅" } else { "❌" },
                        path.file_name()
                            .map(|name| name.to_string_lossy())
                            .unwrap_or_default()
                    );

                    if !status.success() {
                        failed.push(TestError::TestBinaryError);
                    }
                }
            }

            if !failed.is_empty() {
                return Err(
                    Error::TestFailure { fails: failed }.into()
                );
            }
        }
    }
    Ok(())
}
