use std::path::PathBuf;

#[cfg(debug_assertions)]
use regex::Regex;

use crate::cli::BuildCommand;

/**
 * Crate-wide configuration structures.
 */

/// Configuration for a single build run.
pub(crate) struct BuildConfig {
    /// The input file for this build.
    pub input_file: PathBuf,

    /// The output directory for this build.
    pub out_dir: PathBuf,

    /// The output linked executable file for this build.
    pub out_file: PathBuf,

    /// Whether to generate code with security features enabled.
    pub gen_security_features: bool,

    /// Whether to output a parsed representation of the AST.
    /// Available in debug mode only.
    #[cfg(debug_assertions)]
    pub output_ast: bool,

    /// Regex for functions which should have an IR representation printed to console.
    /// Available in debug mode only.
    #[cfg(debug_assertions)]
    pub output_ir_regex: Option<Regex>,
}

impl TryFrom<BuildCommand> for BuildConfig {
    type Error = miette::Report;

    /// Attempts to convert a CLI build command into a valid build configuration.
    fn try_from(cli: BuildCommand) -> Result<Self, Self::Error> {
        // Verify the input file exists.
        if !cli.input.exists() || !cli.input.is_file() {
            miette::bail!("Input file either does not exist, or is not a file.");
        }

        // If an output directory is specified, check if it exists.
        // If it doesn't, attempt to create it.
        let out_dir = cli.output_dir.unwrap_or(PathBuf::from("./out"));
        if out_dir.exists() && !out_dir.is_dir() {
            miette::bail!("config: Output directory exists, but is not a directory.");
        }
        if !out_dir.exists() {
            std::fs::create_dir(out_dir.clone())
                .map_err(|_| miette::diagnostic!("Failed to create output directory."))?;
        }

        // Get the output file name.
        let mut out_file = out_dir.clone();
        match cli.output_name {
            Some(name) => out_file.push(name),
            None => out_file.push(cli.input.file_stem().unwrap()),
        }

        // If there is a regex for printing IR, parse that into a [`Regex`] structure.
        #[cfg(debug_assertions)]
        let output_ir_regex = cli
            .output_ir_for
            .map(|s| Regex::new(&format!("^{}$", s)))
            .transpose()
            .map_err(|e| {
                miette::diagnostic!("Failed to parse regex for IR output function match: {e}")
            })?;

        Ok(BuildConfig {
            input_file: cli.input.clone(),
            out_dir,
            out_file,
            gen_security_features: !cli.disable_security_features,
            #[cfg(debug_assertions)]
            output_ast: cli.output_ast,
            #[cfg(debug_assertions)]
            output_ir_regex,
        })
    }
}
