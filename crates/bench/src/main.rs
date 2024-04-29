use std::env;

use clap::Parser;
use cli::Cli;
use log::BenchmarkLog;
use runner::{run_all, Cfg};

mod bench;
mod cli;
mod log;
mod runner;

fn main() -> miette::Result<()> {
    // Parse out command line options.
    let cfg: Cfg = Cli::parse().try_into()?;

    // Run all provided tests.
    let started_at = chrono::offset::Local::now().to_utc();
    let benchmarks = run_all(&cfg)?;
    let ended_at = chrono::offset::Local::now().to_utc();

    // Output the log file.
    let log = BenchmarkLog {
        started_at,
        ended_at,
        arch: env::consts::ARCH.into(),
        opt_level: cfg.cobalt_opt_level.clone(),
        hw_security_enabled: !cfg.disable_hw_security,
        benchmarks,
    };
    log.write_to(cfg.output_log)?;

    Ok(())
}
