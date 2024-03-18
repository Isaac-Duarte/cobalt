use std::fs;
use miette::Result;

use crate::{cli::BuildCommand, compiler::{codegen, parser}};

/// Builds the provided COBOL file.
pub(crate) fn run_build(args: &BuildCommand) -> Result<()> {
    // Load contents of passed file.
    let txt = fs::read_to_string(args.input()).expect("Failed to load source file from disk.");

    // Perform a parse pass.
    let ast = parser::parse(&txt)?;

    // Generate object.
    let code_gen = codegen::CodeGenerator::new().unwrap();
    code_gen.generate(ast);

    Ok(())
}