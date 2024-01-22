use clap::Parser;
use std::{
    fs,
    path::{Path, PathBuf},
};

mod ast;
mod codegen;
mod ir;
mod parser;

// REMMEMBER! PROTOTYPE!
// This is the bootstrap compiler!!!
//
// This does not need to be nice
// it does not need to have error handling
// it just needs  to work

fn build(input_file: &Path, output_file: &Path) {
    let content = std::fs::read_to_string(input_file).unwrap();
    let ast = parser::parse(&content);
    dbg!(ast);

    todo!()
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
