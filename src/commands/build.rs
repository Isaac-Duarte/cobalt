use std::fs;
use miette::Result;

use crate::{cli::BuildCommand, compiler::{codegen, parser::Parser}};

/// Builds the provided COBOL file.
pub(crate) fn run_build(args: &BuildCommand) -> Result<()> {
    // Load contents of passed file.
    let txt = fs::read_to_string(args.input()).expect("Failed to load source file from disk.");

    // Perform a parse pass.
    let parser = Parser::new(args.input().to_str().unwrap(), &txt);
    let (ast, literals) = parser.parse()?;

    // Translate the AST into Cranelift IR.
    let mut code_gen = codegen::CodeGenerator::new(&ast).expect("Failed to create code generator.");
    code_gen.translate(ast, &literals)?;

    // Write generated object code to file.
    code_gen.generate();

    Ok(())
}