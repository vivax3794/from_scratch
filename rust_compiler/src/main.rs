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
    // clippy::panic_in_result_fn,
    clippy::tests_outside_test_module
)]
use std::path::{Path, PathBuf};

use clap::Parser;
use wait_timeout::ChildExt;

use crate::error::{Error, Result, TestError};

mod ast;
mod codegen;
mod error;
mod ir;
mod lexer;
mod parser;
mod span;
mod type_system;

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
            let content = std::fs::read_to_string(&input).map_err(Error::from)?;
            build_wrapped(content, &input, &output)?;
        }
        Commands::Test { directory } => {
            do_test(directory)?;
        }
    }
    Ok(())
}

/// Run all tests in a directory
fn do_test(directory: PathBuf) -> Result<(), miette::Error> {
    let mut failed = Vec::new();
    let normal_dir = directory.join("normal");
    let xfail_dir = directory.join("xfail");
    println!("===== running normal tests =====");
    for entry in std::fs::read_dir(normal_dir).map_err(Error::from)? {
        let entry = entry.map_err(Error::from)?;
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "viv") {
            let output = tempfile::NamedTempFile::new().map_err(Error::from)?;
            let content = std::fs::read_to_string(&path).map_err(Error::from)?;

            match build_wrapped(content, &path, output.path()) {
                Ok(()) => {}
                Err(err) => {
                    println!(
                        "❌ {}",
                        path.file_name()
                            .map(|name| name.to_string_lossy())
                            .unwrap_or_default()
                    );
                    failed.push(TestError::TestCompileError { error: err });
                    continue;
                }
            }

            let (file, output_path) = output.keep().map_err(Error::from)?;
            drop(file);

            let mut child = std::process::Command::new(&output_path)
                .spawn()
                .map_err(Error::from)?;

            let time = std::time::Duration::from_secs(2);
            let Ok(Some(status)) = child.wait_timeout(time) else {
                println!(
                    "❌ {} - TIMEOUT",
                    path.file_name()
                        .map(|name| name.to_string_lossy())
                        .unwrap_or_default()
                );
                failed.push(TestError::TestTimeout);
                continue;
            };

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
    println!("===== running xfail tests =====");
    for entry in std::fs::read_dir(xfail_dir).map_err(Error::from)? {
        let entry = entry.map_err(Error::from)?;
        let path = entry.path();

        // Check if the entry is a directory
        if path.is_dir() {
            // Now iterate over files in this subdirectory
            for file_entry in std::fs::read_dir(path).map_err(Error::from)? {
                let file_entry = file_entry.map_err(Error::from)?;
                let file_path = file_entry.path();

                // Check if the file has a .viv extension
                if file_path.extension().is_some_and(|ext| ext == "viv") {
                    let output = tempfile::NamedTempFile::new().map_err(Error::from)?;
                    let content = std::fs::read_to_string(&file_path).map_err(Error::from)?;

                    match build_wrapped(content, &file_path, output.path()) {
                        Ok(()) => {
                            println!(
                                "❌ {}/{}",
                                file_path
                                    .parent()
                                    .unwrap_or_else(|| Path::new(""))
                                    .file_name()
                                    .unwrap_or_default()
                                    .to_string_lossy(),
                                file_path.file_name().unwrap_or_default().to_string_lossy()
                            );
                            failed.push(TestError::TestXfail);
                            continue;
                        }
                        Err(err) => {
                            println!(
                                "✅ {}/{}: {err}",
                                file_path
                                    .parent()
                                    .unwrap_or_else(|| Path::new(""))
                                    .file_name()
                                    .unwrap_or_default()
                                    .to_string_lossy(),
                                file_path.file_name().unwrap_or_default().to_string_lossy()
                            );
                        }
                    }
                }
            }
        }
    }
    Ok(if !failed.is_empty() {
        return Err(Error::TestFailure { fails: failed }.into());
    })
}
