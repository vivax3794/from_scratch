#![feature(map_try_insert)]

use clap::Parser;
use std::path::{Path, PathBuf};

mod ast;
mod codegen;
mod ir;
mod parser;
mod type_system;

// REMMEMBER! PROTOTYPE!
// This is the bootstrap compiler!!!
//
// This does not need to be nice
// it does not need to have error handling
// it just needs  to work

fn build(input_file: &Path, output_file: &Path) {
    let content = std::fs::read_to_string(input_file).unwrap();
    let ast = parser::parse(&content);

    let mut type_resolver = type_system::TypeResolver::new();
    let ir = type_resolver.resolve_file(&ast);

    let context = inkwell::context::Context::create();
    let mut gen = codegen::CodeGen::new(&context);
    gen.generate_file(&ir);

    let llvm_ir = tempfile::NamedTempFile::new().unwrap();
    let object_file = tempfile::NamedTempFile::new().unwrap();
    gen.save_to_file(llvm_ir.path());
    std::process::Command::new("llc")
        .args([
            llvm_ir.path().to_str().unwrap(),
            "-filetype=obj",
            "-o",
            object_file.path().to_str().unwrap(),
        ])
        .status()
        .unwrap();
    std::process::Command::new("gcc")
        .args([
            object_file.path().to_str().unwrap(),
            "-no-pie",
            "-o",
            output_file.to_str().unwrap(),
        ])
        .status()
        .unwrap();
}

#[derive(Parser)]
enum Commands {
    Build { input: PathBuf, output: PathBuf },
}

fn main() {
    let commands = Commands::parse();
    match commands {
        Commands::Build { input, output } => build(&input, &output),
    }
}
