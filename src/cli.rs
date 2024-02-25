use std::path::PathBuf;
use clap::{Parser, Subcommand};

/// Top level CLI options for the compiler.
#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// The command to be run.
    #[command(subcommand)]
    command: Command,
}

impl Cli {
    pub fn command(&self) -> &Command {
        &self.command
    }
}

/// All available subcommands within cobalt.
#[derive(Subcommand)]
pub enum Command {
    Build(BuildCommand)
}

#[derive(Parser)]
#[command(about = "Builds a single COBOL file.")]
pub struct BuildCommand {
    /// The file to build.
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Set the verbosity of compiler output. Can be specified
    /// multiple times.
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8
}

impl BuildCommand {
    pub fn input(&self) -> &PathBuf {
        &self.input
    }

    pub fn verbose(&self) -> u8 {
        self.verbose
    }
}