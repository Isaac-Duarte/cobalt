use clap::Parser;
use cli::Cli;
use runner::{run_all, Cfg};

mod bench;
mod cli;
mod runner;

fn main() -> miette::Result<()> {
    // Parse out command line options.
    let cfg: Cfg = Cli::parse().try_into()?;

    // Run all provided tests.
    run_all(&cfg)?;

    Ok(())
}
