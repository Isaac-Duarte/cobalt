use std::{io::Write, path::PathBuf, time::Duration};

use chrono::{DateTime, Utc};
use miette::Result;
use serde::Serialize;

use crate::bench::Benchmark;

/// The top-level output log for the benchmarking program.
/// Contains information about all benchmarks performed, and their execution times.
#[derive(Serialize)]
pub(crate) struct BenchmarkLog {
    /// The time at which the benchmarking run started, in UTC time.
    pub started_at: DateTime<Utc>,

    /// The time at which the benchmarking run ended, in UTC time.
    pub ended_at: DateTime<Utc>,

    /// The architecture on which the benchmarking was performed.
    pub arch: String,

    /// The level of optimisations used for Cobalt.
    pub opt_level: String,

    /// Whether hardware security instructions were enabled or not.
    pub hw_security_enabled: bool,

    /// Output for the benchmarks executed.
    pub benchmarks: Vec<BenchmarkExecution>,
}

impl BenchmarkLog {
    /// Outputs this benchmark log to the given filepath.
    pub fn write_to(self, f: PathBuf) -> Result<()> {
        let output = serde_json::to_string_pretty(&self)
            .map_err(|e| miette::diagnostic!("Failed to write benchmark log to file: {e}"))?;
        {
            let mut log = std::fs::OpenOptions::new()
                .read(true)
                .write(true) // <--------- this
                .create(true)
                .open(f)
                .map_err(|e| {
                    miette::diagnostic!("Failed to open file handle for benchmark log file: {e}")
                })?;
            write!(&mut log, "{}", output).map_err(|e| {
                miette::diagnostic!("Failed to write contents to benchmark log file: {e}")
            })?;
        }
        Ok(())
    }
}

/// Output for a single benchmark execution.
#[derive(Serialize)]
pub(crate) struct BenchmarkExecution {
    /// The benchmark being executed.
    pub benchmark: Benchmark,

    /// The time at which this benchmark begun execution.
    pub started_at: DateTime<Utc>,

    /// The time at which this benchmark completed execution.
    pub ended_at: DateTime<Utc>,

    /// Benchmark results for `cobalt`.
    pub cobalt_results: BenchmarkResult,

    /// Benchmark results for `cobc`, if present.
    /// Only generated when executed with `--run-comparative`.
    pub cobc_results: Option<BenchmarkResult>,
}

/// Output for the result of a single benchmark compile/execute pair.
/// Can be for either `cobalt` or `cobc`.
#[derive(Serialize)]
pub(crate) struct BenchmarkResult {
    /// The time taken to compile the benchmark for the specified
    /// iterations.
    pub compile_time_total: Duration,

    /// The time taken to compile the benchmark once, on average.
    pub compile_time_avg: Duration,

    /// The time taken to execute the benchmark for the specified
    /// iterations. Not present if no execution was performed.
    pub execute_time_total: Option<Duration>,

    /// The time taken to execute the benchmark once, on average.
    /// Not present if no execution was performed.
    pub execute_time_avg: Option<Duration>,
}
