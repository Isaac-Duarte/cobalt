use std::{path::PathBuf, str::FromStr};

use miette::Result;
use serde::Deserialize;

/// Represents a single benchmark to be executed by the benchmark runner.
/// Included benchmarks are stored as `.toml` files within the `/benchmarks`
/// directory at the root of the repository.
#[derive(Deserialize)]
pub(crate) struct Benchmark {
    /// The display name of this benchmark.
    pub name: String,

    /// Path to the source file for this benchmark.
    /// If relative, begins at the working directory of the benchmarker.
    pub source_file: String,

    /// The `stdin` input to pipe to the program on each run of the benchmark.
    /// If not present, no input is passed.
    pub stdin: Option<String>,

    /// The number of iterations to run for this benchmark.
    pub iterations: u64,
}

impl Benchmark {
    /// Verifies that the benchmark data is valid for execution.
    pub fn verify(&self) -> Result<()> {
        // Check the source file exists.
        if !PathBuf::from_str(&self.source_file)
            .map_err(|_| {
                miette::diagnostic!(
                    "Invalid path '{}' provided in benchmark file.",
                    &self.source_file
                )
            })?
            .exists()
        {
            miette::bail!(
                "Path '{}' provided in benchmark file does not exist.",
                &self.source_file
            );
        }

        // Check the number of iterations is >= 1.
        if self.iterations < 1 {
            miette::bail!("Number of iterations for a benchmark must be >= 1.");
        }

        Ok(())
    }
}
