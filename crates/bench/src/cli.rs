use std::{env, io::ErrorKind, path::PathBuf, process::Command};

use clap::Parser;
use colored::Colorize;

use crate::{bench::Benchmark, runner::Cfg};

/// Top level CLI options for the benchmarker.
#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// The location of the Cobalt binary to test.
    /// By default, `./build/cobalt`.
    #[arg(short = 'c', long, value_name = "COMPILER")]
    pub compiler: Option<PathBuf>,

    /// Whether to run comparative tests against GnuCobol.
    #[arg(long, short = 'g', action)]
    pub run_comparative: bool,

    /// The output directory for testbench results.
    /// This should ideally not contain other unrelated files or folders.
    #[arg(short = 'o', long, value_name = "OUT_DIR")]
    pub output_dir: Option<PathBuf>,

    /// The input directory for benchmarks to run.
    /// By default, `./benchmarks`.
    #[arg(short = 'd', long, value_name = "IN_DIR")]
    pub benchmark_dir: Option<PathBuf>,

    /// A list of benchmark files to run.
    /// Executed in addition to any benchmarks found in the provided benchmark
    /// directory, if present.
    #[arg(short = 'b', long)]
    pub benchmarks: Option<Vec<PathBuf>>,
}

impl TryInto<Cfg> for Cli {
    type Error = miette::Report;

    /// Attempts to convert the CLI arguments given in to a build configuration.
    fn try_into(self) -> Result<Cfg, Self::Error> {
        // Verify the existence of the target Cobalt binary.
        let cobalt_bin = if let Some(path) = self.compiler {
            path
        } else {
            let mut path = env::current_dir().unwrap();
            path.push("build");
            path.push("cobalt");
            path
        };
        if !cobalt_bin.exists() {
            miette::bail!(
                "Target Cobalt binary '{}' does not exist.",
                cobalt_bin.to_str().unwrap()
            );
        }

        // If running comparative tests is enabled, ensure that GnuCobol's `cobc` is available.
        if self.run_comparative {
            match Command::new("cobc").output() {
                Ok(_) => {}
                Err(e) => {
                    if let ErrorKind::NotFound = e.kind() {
                        miette::bail!(
                            "Comparative tests enabled, but `cobc` was not found! Check your PATH."
                        );
                    } else {
                        miette::bail!(
                            "An error occurred while verifying if `cobc` was available: {}",
                            e
                        );
                    }
                }
            }
        }

        // Verify that the output directory exists.
        let output_dir = if let Some(path) = self.output_dir {
            path
        } else {
            let mut path = env::current_dir().unwrap();
            path.push("bench_out");
            path
        };
        if !output_dir.exists() {
            std::fs::create_dir(output_dir.clone())
                .map_err(|e| miette::diagnostic!("Failed to create output directory: {e}"))?;
        }

        // If there's an input directory, scan it for any input benchmark `.toml` files.
        let mut benchmark_candidates: Vec<PathBuf> = Vec::new();
        let mut all_benchmarks: Vec<Benchmark> = Vec::new();
        let input_dir = if let Some(input_dir) = self.benchmark_dir {
            if !input_dir.exists() {
                miette::bail!(
                    "Provided benchmark directory '{}' does not exist.",
                    input_dir.to_str().unwrap()
                );
            }
            input_dir
        } else {
            let mut path = env::current_dir().unwrap();
            path.push("benchmarks");
            path
        };
        if input_dir.exists() {
            benchmark_candidates.append(
                &mut std::fs::read_dir(input_dir)
                    .map_err(|e| {
                        miette::diagnostic!("Failed to read from provided benchmark directory: {e}")
                    })?
                    .filter_map(|res| res.ok())
                    .map(|dir_entry| dir_entry.path())
                    .filter_map(|path| {
                        if path.extension().map_or(false, |ext| ext == "toml") {
                            Some(path)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>(),
            );
        }

        // Add all benchmark files passed separately.
        if let Some(mut additional_files) = self.benchmarks {
            benchmark_candidates.append(&mut additional_files);
        }

        // Attempt to load all benchmark candidate files.
        for candidate in benchmark_candidates {
            let benchmark = toml::from_str::<Benchmark>(
                &std::fs::read_to_string(candidate.clone()).map_err(|e| {
                    miette::diagnostic!(
                        "Failed to read benchmark file '{}': {e}",
                        candidate.to_str().unwrap()
                    )
                })?,
            );
            match benchmark {
                Ok(inner) => {
                    // Verify that the benchmark is sane.
                    inner.verify().map_err(|e| {
                        miette::diagnostic!(
                            "Failed to validate benchmark file '{}': {e}",
                            candidate.to_str().unwrap()
                        )
                    })?;
                    all_benchmarks.push(inner);
                }
                Err(e) => {
                    println!(
                        "{}{}{}{}",
                        "warning: Failed to load benchmark file '".yellow(),
                        candidate.to_str().unwrap().yellow(),
                        "', parse error: ".yellow(),
                        e
                    );
                }
            }
        }

        // If there are no benchmarks to run, exit now.
        if all_benchmarks.len() == 0 {
            miette::bail!("No valid benchmarks discovered to run, exiting.");
        }

        // Build the configuration for running tests.
        Ok(Cfg {
            compiler: cobalt_bin,
            run_comparative: self.run_comparative,
            output_dir,
            benchmarks: all_benchmarks,
        })
    }
}