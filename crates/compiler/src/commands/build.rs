use miette::Result;
use std::fs;

use crate::{
    cli::BuildCommand,
    compiler::{codegen, parser::Parser},
    config::BuildConfig,
    linker::Linker,
};

/// Builds the provided COBOL file.
pub(crate) fn run_build(args: BuildCommand) -> Result<()> {
    // Create a build configuration from the passed arguments.
    let cfg = BuildConfig::try_from(args)?;

    // Load contents of passed file.
    let txt = fs::read_to_string(&cfg.input_file).expect("Failed to load source file from disk.");

    // Perform a parse pass.
    let parser = Parser::new(cfg.input_file.to_str().unwrap(), &txt);
    let ast = parser.parse()?;
    #[cfg(debug_assertions)]
    if cfg.output_ast {
        println!("info(ast): {:#?}", ast);
    }

    // Translate the AST into Cranelift IR.
    let mut code_gen =
        codegen::CodeGenerator::new(&cfg, ast).expect("Failed to create code generator.");
    code_gen.translate()?;

    // Write generated object code to file.
    let obj_path = code_gen.generate()?;

    // Link.
    let mut linker = Linker::new(&cfg)?;
    linker.add_object(obj_path);
    linker.link()?;

    Ok(())
}
