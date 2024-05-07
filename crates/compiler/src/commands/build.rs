#[cfg(debug_assertions)]
use colored::Colorize;
use miette::Result;
use std::fs;

use crate::{
    cli::BuildCommand,
    compiler::{codegen, parser::Parser},
    config::BuildConfig,
    linker::Linker,
};

/// Executes the given build command, building all passed files.
pub(crate) fn run_build(args: BuildCommand) -> Result<()> {
    // Create a build configuration from the passed arguments.
    let cfg = BuildConfig::try_from(args)?;

    // Load contents of passed file, attempt build.
    let txt = fs::read_to_string(&cfg.input_file).expect("Failed to load source file from disk.");
    build_file(&txt, &cfg)
}

/// Builds the provided source COBOL file, producing an output executable.
pub(crate) fn build_file(source: &str, cfg: &BuildConfig) -> Result<()> {
    // Perform a parse pass.
    let parser = Parser::new(cfg.input_file.to_str().unwrap(), source);
    let ast = parser.parse()?;
    #[cfg(debug_assertions)]
    if cfg.output_ast {
        println!("{}{:#?}", "info(ast): ".blue(), ast);
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