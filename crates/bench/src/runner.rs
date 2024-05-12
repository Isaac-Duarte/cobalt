use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
    time::{Duration, Instant},
};

use colored::Colorize;
use miette::Result;
use regex::Regex;

use crate::{
    bench::Benchmark,
    log::{BenchmarkExecution, BenchmarkResult},
};

/// The name of the benchmarking output binary.
const BENCH_BIN_NAME: &str = "bench_bin";

/// Configuration for executing benchmarks.
pub(crate) struct Cfg {
    /// The Cobalt binary to use for compilation.
    pub compiler: PathBuf,

    /// The optimisation level to run Cobalt at when compiling.
    pub cobalt_opt_level: String,

    /// The optimisation level to run `cobc` at when compiling.
    pub cobc_opt_level: u8,

    /// Whether to force use of the platform linker when using `cobc`.
    pub cobc_force_platform_linker: bool,

    /// Whether to disable generating hardware security instructions
    /// when compiling sources with Cobalt.
    pub disable_hw_security: bool,

    /// Whether to run comparative tests against GnuCobol's `cobc`.
    pub run_cobc: bool,

    /// Whether to run comparative tests against `cobc` -> `clang`.
    pub run_clang: bool,

    /// Whether to only build and not execute benchmarks.
    pub build_only: bool,

    /// The output directory for benchmark artifacts.
    pub output_dir: PathBuf,

    /// The file to output benchmark log information to.
    pub output_log: PathBuf,

    /// The benchmarks to execute during this run.
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
    let cobc_results = cfg
        .run_cobc
        .then(|| run_cobc(cfg, benchmark))
        .transpose()?;
    let clang_results = cfg
        .run_clang
        .then(|| run_clang(cfg, benchmark))
        .transpose()?;

    Ok(BenchmarkExecution {
        benchmark: benchmark.clone(),
        started_at,
        ended_at: chrono::offset::Local::now().to_utc(),
        cobalt_results,
        cobc_results,
        clang_results,
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
        .args(["--opt-level", &cfg.cobalt_opt_level])
        .args(["--output-dir", cfg.output_dir.to_str().unwrap()])
        .args(["--output-name", BENCH_BIN_NAME]);
    if cfg.cobc_force_platform_linker {
        cobalt.arg("--prefer-platform-linker");
    }
    if cfg.disable_hw_security {
        cobalt.arg("--disable-security-features");
    }

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
    let (execute_time_total, execute_time_avg) = if !cfg.build_only {
        let (x, y) = run_bench_bin(cfg, benchmark)?;
        (Some(x), Some(y))
    } else {
        (None, None)
    };

    Ok(BenchmarkResult {
        compile_time_total: elapsed,
        compile_time_avg: elapsed / 100,
        execute_time_total,
        execute_time_avg,
    })
}

/// Executes a single benchmark using GnuCobol's `cobc`.
fn run_cobc(cfg: &Cfg, benchmark: &Benchmark) -> Result<BenchmarkResult> {
    // Build the target program with `cobc`.
    let mut bench_bin_path = cfg.output_dir.clone();
    bench_bin_path.push(BENCH_BIN_NAME);
    let mut cobc = Command::new("cobc");
    cobc.args(["-x", &format!("-O{}", cfg.cobc_opt_level), "-free"])
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
    let (execute_time_total, execute_time_avg) = if !cfg.build_only {
        let (x, y) = run_bench_bin(cfg, benchmark)?;
        (Some(x), Some(y))
    } else {
        (None, None)
    };

    Ok(BenchmarkResult {
        compile_time_total: elapsed,
        compile_time_avg: elapsed / 100,
        execute_time_total,
        execute_time_avg,
    })
}

/// Executes a single benchmark using GnuCobol's `cobc`'s C output followed by
/// binary generation using `clang`.
fn run_clang(cfg: &Cfg, benchmark: &Benchmark) -> Result<BenchmarkResult> {
    // Calculate output file locations for benchmarking binary, C file.
    // We also need a bootstrapping file for `main` since GnuCobol doesn't create
    // one for us.
    let mut bench_bin_path = cfg.output_dir.clone();
    bench_bin_path.push(BENCH_BIN_NAME);
    let mut bench_c_path = cfg.output_dir.clone();
    bench_c_path.push(format!("{}.c", &benchmark.name));
    let mut bootstrap_path = cfg.output_dir.clone();
    bootstrap_path.push("bootstrap.c");

    // Set up commands for transpiling then compiling from C.
    let mut cobc = Command::new("cobc");
    cobc.args(["-C", &format!("-O{}", cfg.cobc_opt_level), "-free"])
        // Required to generate `cob_init()` in the transpiled C.
        .arg("-fimplicit-init")
        .args(["-o", bench_c_path.to_str().unwrap()])
        .arg(&benchmark.source_file);
    let mut clang = Command::new("clang");
    clang
        .args(["-lcob", &format!("-O{}", cfg.cobc_opt_level)])
        .args(["-o", bench_bin_path.to_str().unwrap()])
        .arg(bootstrap_path.to_str().unwrap());

    // We require the program ID of the COBOL file to generate the bootstrapper.
    // Attempt to grep for that from the source.
    let bench_prog_func = fetch_program_func(benchmark)?;

    // Generate the bootstrapping file.
    let bootstrapper = format!(
        "
        #include \"{}.c\"
        int main(void) {{
            {}();
        }}
    ",
        &benchmark.name, bench_prog_func
    );
    std::fs::write(bootstrap_path.clone(), bootstrapper)
        .map_err(|e| miette::diagnostic!("Failed to write bootstrap file for `clang`: {e}"))?;

    let before = Instant::now();
    for _ in 0..100 {
        // First, transpile to C.
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

        // Finally, perform compilation & linkage with `clang`.
        let out = clang
            .output()
            .map_err(|e| miette::diagnostic!("Failed to execute `clang`: {e}"))?;
        if !out.status.success() {
            miette::bail!(
                "Failed benchmark for '{}' with `clang` compiler error: {}",
                benchmark.source_file,
                String::from_utf8_lossy(&out.stderr)
            );
        }
    }
    let elapsed = before.elapsed();
    println!(
        "clang(compile): Total time {:.2?}, average/run of {:.6?}.",
        elapsed,
        elapsed / 100
    );

    // Run the target program.
    let (execute_time_total, execute_time_avg) = if !cfg.build_only {
        let (x, y) = run_bench_bin(cfg, benchmark)?;
        (Some(x), Some(y))
    } else {
        (None, None)
    };

    Ok(BenchmarkResult {
        compile_time_total: elapsed,
        compile_time_avg: elapsed / 100,
        execute_time_total,
        execute_time_avg,
    })
}

/// Attempts to fetch the output C function name of the given benchmark COBOL file.
/// If not found, throws an error.
fn fetch_program_func(benchmark: &Benchmark) -> Result<String> {
    // First, read in source of benchmark.
    let source = std::fs::read_to_string(&benchmark.source_file).map_err(|e| {
        miette::diagnostic!(
            "Failed to read COBOL source for benchmark '{}': {e}",
            benchmark.name
        )
    })?;

    // Search for a pattern matching "PROGRAM-ID ...".
    let prog_id_pat = Regex::new(r"PROGRAM-ID\. [A-Z0-9a-z\-]+").unwrap();
    let prog_id_str = prog_id_pat.find(&source).ok_or(miette::diagnostic!(
        "Could not find program ID in sources for benchmark '{}'.",
        &benchmark.name
    ))?;

    // Extract the program ID, format into final function name.
    let mut prog_id = prog_id_str.as_str()["PROGRAM-ID ".len()..].to_string();
    prog_id = prog_id.replace("-", "__");
    Ok(prog_id)
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
