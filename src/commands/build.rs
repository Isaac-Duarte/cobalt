use std::fs;
use miette::Result;

use crate::{cli::BuildCommand, compiler::parser};

/// Builds the provided COBOL file.
pub(crate) fn run_build(args: &BuildCommand) -> Result<()> {
    // Load contents of passed file.
    let txt = fs::read_to_string(args.input()).expect("Failed to load source file from disk.");

    // Perform a parse pass.
    parser::parse(&txt)?;

    // Generate Cranelift IR from AST.

    // Compile Cranelift IR down to object file.

    Ok(())
}