use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
    time::{Duration, Instant},
};

use colored::Colorize;
use miette::Result;

use crate::{bench::Benchmark, log::{BenchmarkExecution, BenchmarkResult}};

/// The name of the benchmarking output binary.
const BENCH_BIN_NAME: &str = "bench_bin";

/// Configuration for executing benchmarks.
pub(crate) struct Cfg {
    pub compiler: PathBuf,
    pub run_comparative: bool,
    pub output_dir: PathBuf,
    pub output_log: PathBuf,
    pub benchmarks: Vec<Benchmark>,
}

/// Runs all benchmarks specified in the provided configuration.
pub(crate) fn run_all(cfg: &Cfg) -> Result<Vec<BenchmarkExecution>> {
    let mut executions = Vec::new();
    for benchmark in cfg.benchmarks.iter() {
        executions.push(run_single(cfg, benchmark)?);
    }
    Ok(executions)
}

/// Executes a single benchmark.
pub(crate) fn run_single(cfg: &Cfg, benchmark: &Benchmark) -> Result<BenchmarkExecution> {
    let started_at = chrono::offset::Local::now().to_utc();
    println!(
        "\n=== benchmark: {} ({} iters) === ",
        benchmark.name.as_str().bold(),
        benchmark.iterations
    );
    let cobalt_results = run_cobalt(cfg, benchmark)?;
    let cobc_results = cfg.run_comparative.then(|| run_cobc(cfg, benchmark)).transpose()?;

    Ok(BenchmarkExecution {
        benchmark: benchmark.clone(),
        started_at,
        ended_at: chrono::offset::Local::now().to_utc(),
        cobalt_results,
        cobc_results
    })
}

/// Executes a single benchmark using Cobalt.
fn run_cobalt(cfg: &Cfg, benchmark: &Benchmark) -> Result<BenchmarkResult> {
    // Build the target program with Cobalt.
    const BENCH_BIN_NAME: &str = "bench_bin";
    let mut cobalt = Command::new(cfg.compiler.to_str().unwrap());
    cobalt
        .arg("build")
        .arg(&benchmark.source_file)
        .args(["--output-dir", cfg.output_dir.to_str().unwrap()])
        .args(["--output-name", BENCH_BIN_NAME]);

    let before = Instant::now();
    for _ in 0..100 {
        let out = cobalt
            .output()
            .map_err(|e| miette::diagnostic!("Failed to execute Cobalt: {e}"))?;
        if !out.status.success() {
            miette::bail!(
                "Failed benchmark for '{}' with Cobalt compiler error: {}",
                benchmark.source_file,
                String::from_utf8_lossy(&out.stderr)
            );
        }
    }
    let elapsed = before.elapsed();
    println!(
        "cobalt(compile): Total time {:.2?}, average/run of {:.6?}.",
        elapsed,
        elapsed / 100
    );

    // Run the target program.
    let (execute_time_total, execute_time_avg) = run_bench_bin(cfg, benchmark)?;

    Ok(BenchmarkResult {
        compile_time_total: elapsed,
        compile_time_avg: elapsed / 100,
        execute_time_total,
        execute_time_avg
    })
}

/// Executes a single benchmark using GnuCobol's `cobc`.
fn run_cobc(cfg: &Cfg, benchmark: &Benchmark) -> Result<BenchmarkResult> {
    // Build the target program with `cobc`.
    let mut bench_bin_path = cfg.output_dir.clone();
    bench_bin_path.push(BENCH_BIN_NAME);
    let mut cobc = Command::new("cobc");
    cobc.args(["-x", "-O3", "-free"])
        .args(["-o", bench_bin_path.to_str().unwrap()])
        .arg(&benchmark.source_file);

    let before = Instant::now();
    for _ in 0..100 {
        let out = cobc
            .output()
            .map_err(|e| miette::diagnostic!("Failed to execute `cobc`: {e}"))?;
        if !out.status.success() {
            miette::bail!(
                "Failed benchmark for '{}' with `cobc` compiler error: {}",
                benchmark.source_file,
                String::from_utf8_lossy(&out.stderr)
            );
        }
    }
    let elapsed = before.elapsed();
    println!(
        "cobc(compile): Total time {:.2?}, average/run of {:.6?}.",
        elapsed,
        elapsed / 100
    );

    // Run the target program.
    let (execute_time_total, execute_time_avg) = run_bench_bin(cfg, benchmark)?;

    Ok(BenchmarkResult {
        compile_time_total: elapsed,
        compile_time_avg: elapsed / 100,
        execute_time_total,
        execute_time_avg
    })
}

/// Executes a single generated benchmarking binary.
/// Returns the total execution time and average execution time per iteration.
fn run_bench_bin(cfg: &Cfg, benchmark: &Benchmark) -> Result<(Duration, Duration)> {
    let mut bench_bin_path = cfg.output_dir.clone();
    bench_bin_path.push(BENCH_BIN_NAME);

    // Prefer not passing `stdin` if possible, as there is less overhead.
    let elapsed = if let Some(input) = &benchmark.stdin {
        run_bin_stdin(&bench_bin_path, benchmark.iterations, input)?
    } else {
        run_bin_nostdin(&bench_bin_path, benchmark.iterations)?
    };
    println!(
        "bench(run): Total time {:.2?}, average/run of {:.6?}.",
        elapsed,
        elapsed / benchmark.iterations as u32
    );
    Ok((elapsed, elapsed / benchmark.iterations as u32))
}

/// Executes the given binary for `iters` iterations, without passing input via. `stdin`.
/// Returns the duration that it took to execute the given number of iterations.
fn run_bin_nostdin(bin: &PathBuf, iters: u64) -> Result<Duration> {
    let mut cmd = Command::new(bin.to_str().unwrap());
    let before = Instant::now();
    for _ in 0..iters {
        cmd.output()
            .map_err(|e| miette::diagnostic!("Failed to execute bench binary: {e}"))?;
    }
    Ok(before.elapsed())
}

/// Executes the given binary for `iters` iterations, passing the provided input
/// via. `stdin` on each invocation. Returns the duration that it took to execute
/// the given number of iterations.
fn run_bin_stdin(bin: &PathBuf, iters: u64, input: &str) -> Result<Duration> {
    let mut cmd = Command::new(bin.to_str().unwrap());
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    let input_bytes = input.as_bytes();

    let before = Instant::now();
    for _ in 0..iters {
        let mut child = cmd
            .spawn()
            .map_err(|e| miette::diagnostic!("Failed to spawn child bench process: {e}"))?;
        if let Some(mut child_stdin) = child.stdin.take() {
            child_stdin.write_all(input_bytes).unwrap();
        }
        child
            .wait_with_output()
            .map_err(|e| miette::diagnostic!("Failed to execute child bench process: {e}"))?;
    }
    Ok(before.elapsed())
}
