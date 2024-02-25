use clap::Parser;
use cli::Command;

mod cli;
mod commands;
mod compiler;

fn main() -> miette::Result<()> {
    let cli = cli::Cli::parse();
    match cli.command() {
        Command::Build(args) => commands::build(args)?
    }

    Ok(())
}
