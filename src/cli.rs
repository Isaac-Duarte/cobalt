use std::path::PathBuf;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// The command to be run.
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    Build {
        /// Set the verbosity of compiler output. Can be specified
        /// multiple times.
        #[arg(short, long, action = clap::ArgAction::Count)]
        verbose: u8
    }
}