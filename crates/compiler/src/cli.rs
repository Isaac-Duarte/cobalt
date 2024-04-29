use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Top level CLI options for the compiler.
#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// The command to be run.
    #[command(subcommand)]
    command: Command,
}

impl Cli {
    pub fn command(self) -> Command {
        self.command
    }
}

/// All available subcommands within cobalt.
#[derive(Subcommand)]
pub enum Command {
    Build(BuildCommand),
}

#[derive(Parser)]
#[command(about = "Builds a single COBOL file.")]
pub struct BuildCommand {
    /// The file to build.
    #[arg(value_name = "FILE")]
    pub input: PathBuf,

    /// The name of the output executable.
    /// By default, the primary input file name with extensions removed.
    #[arg(short, long)]
    pub output_name: Option<String>,

    /// The output directory to save to.
    /// By default, `./out`.
    #[arg(short = 'd', long, value_name = "DIR")]
    pub output_dir: Option<PathBuf>,

    /// Set the verbosity of compiler output. Can be specified
    /// multiple times.
    #[arg(short, long, action = clap::ArgAction::Count)]
    pub verbose: u8,

    /// The optimisation level to compile the provided code at.
    /// By default, optimises for compile speed (no optimisations).
    #[arg(short = 'O', long, value_parser = clap::builder::PossibleValuesParser::new(&["none", "speed", "speed_and_size"]))]
    pub opt_level: Option<String>,

    /// Disables the generation of instructions utilising hardware security
    /// features within output binaries (e.g. PAC/BTI on aarch64).
    #[arg(long, action)]
    pub disable_security_features: bool,

    /// Outputs a formatted representation of the parsed AST to console when
    /// enabled. Useful as a debugging tool.
    #[cfg(debug_assertions)]
    #[arg(long, action)]
    pub output_ast: bool,

    /// Outputs an intermediate IR representation of the code for paragraphs
    /// with names matching the given regex. For an anonymous entry paragraph,
    /// supply the string `cobalt::entrypoint`. For the binary entrypoint, supply
    /// `cobalt::main`. Absolute start and end tokens (^|$) are implicit and not required.
    #[cfg(debug_assertions)]
    #[arg(long, value_name = "REGEX")]
    pub output_ir_for: Option<String>,
}
