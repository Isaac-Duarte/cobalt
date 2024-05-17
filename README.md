<div align="center">
<img src="logo.png" width="400"/>

*An experimental COBOL compiler, written in Rust.*
</div>

## Overview

> 🚧 **This project is experimental and not ready for production use. Read the [disclaimer](#disclaimer) for more details.** 🚧

Cobalt is an experimental COBOL compiler targeting x86 and aarch64 Linux systems, providing blazing fast compile times and runtime performance. Some key features include:

* Built on Rust and Cranelift for fast modern code generation.
* Small, efficient output binaries.
* Security by default, utilising hardware features such as PAC+BTI/Intel CET where available. 
* Supports alternative linkers such as `gold` and `mold` for link-time performance.

For a peek inside the internals of Cobalt, see this project's [documentation](). A basic usage outline and performance comparison can be found below.


## Building
This project depends on the presence of a Rust toolchain, with an MSRV of 1.76. The build process may also install the `rust-src` toolchain component, if not already present.

To build the compiler in release mode, simply run the following:

```sh
git clone git@github.com:c272/cobalt.git
cd cobalt && ./build.sh release
```

Build artifacts for the project can be found in the `./build` directory. For testing debug builds during development of the compiler, the "run" and "clean" utilities may also be useful, for example:

```sh
# Build and execute an example with all opts.
./run.sh build examples/algorithms/primality.cobol -O speed_and_size
./out/primality

# Clean up any compiler build artifacts.
./clean.sh
```

## Usage
```
Builds a single COBOL file.

Usage: cobalt build [OPTIONS] <FILE>

Arguments:
  <FILE>  The file to build

Options:
  -o, --output-name <OUTPUT_NAME>  The name of the output executable. By default, the primary input file name with extensions removed
  -d, --output-dir <DIR>           The output directory to save to. By default, `./out`
  -v, --verbose...                 Set the verbosity of compiler output. Can be specified multiple times
  -O, --opt-level <OPT_LEVEL>      The optimisation level to compile the provided code at. By default, optimises for compile speed (no optimisations) [possible values: none, speed, speed_and_size]
  -p, --prefer-platform-linker     Actively selects the platform linker over other available linkers
      --disable-security-features  Disables the generation of instructions utilising hardware security features within output binaries (e.g. PAC/BTI on aarch64)
  -h, --help                       Print help
```

## Benchmarks
A basic set of benchmarks are available in this repository for testing performance against a baseline solution (GnuCobol). These can be run through the `benchmarking` crate, which requires the following additional dependencies:

* When comparing against GnuCobol, `cobc` in PATH. 
* When comparing against GnuCobol+clang, `clang` in PATH.

Results from benchmarking runs can be found, by default, within the `./bench_out` directory as JSON files. For documentation on the structure of these output files, as well as how benchmark TOML files are formatted, view the [`bench` crate documentation]().

Some example uses of the benchmarking suite can be seen below:

```sh
# Display a manual page for the benchmarking tool.
./benchmark.sh --help

# Execute all benchmarks with no comparisons, all opts.
./benchmark.sh -O speed_and_size

# Execute a specific benchmark TOML file.
./benchmark.sh --ignore-default --benchmarks ./benchmarks/foo.toml

# Compare all against `cobc` performance, all opts.
./benchmark.sh --run-cobc -O speed_and_size -C 3

# Compare against `cobc`+`clang`, build performance only.
./benchmark.sh --run-cobc --run-clang --build-only
```

## Disclaimer
This compiler is experimental and feature incomplete, only supporting a minimal set of the COBOL specification. In addition, the only format supported for parity is GnuCobol's `-free` format.

In accordance, this compiler is not intended for production use and should not be used as such. No support is provided for production use cases.