use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
    str::FromStr,
};

use crate::config::BuildConfig;

/// Helper for executing common compiler conformance tests
/// within the unit testing framework.
pub struct CommonTestRunner {
    /// The name of this test.
    name: &'static str,

    /// The input to pass to the compiler as a source file.
    input: &'static str,

    /// The expected output type.
    expected: ExpectedOutput,
}

/// Represents a single expected output from a common compiler
/// conformance test.
pub enum ExpectedOutput {
    /// Expect nothing, run the test unconditionally.
    /// Only fails on a compiler panic.
    None,

    /// Compile fails with output.
    CompileFailure {
        /// An expected reason.
        reason: Option<&'static str>,
    },

    /// The compilation succeeds, no output tested.
    CompilePass,

    /// The compilation succeeds, the output binary produces
    /// some well known output.
    ProgramOutput {
        input: Option<&'static str>,
        expected: &'static str,
    },
}

impl CommonTestRunner {
    /// Creates a new common test runner.
    pub fn new(name: &'static str) -> Self {
        CommonTestRunner {
            name,
            input: "",
            expected: ExpectedOutput::None,
        }
    }

    /// Sets the input source file as the given static text.
    pub fn source(mut self, source: &'static str) -> Self {
        self.input = source;
        self
    }

    /// Modifies the current test runner to expect a compile failure, with an optional
    /// reason provided.
    pub fn expect_fail(mut self, reason: Option<&'static str>) -> Self {
        self.expected = ExpectedOutput::CompileFailure { reason };
        self
    }

    /// Modifies the current test runner to expect a compile failure with no output testing.
    pub fn expect_pass(mut self) -> Self {
        self.expected = ExpectedOutput::CompilePass;
        self
    }

    /// Modifies the current test runner to expect a compile pass, with the output program
    /// producing the given output.
    pub fn expect_output(mut self, output: &'static str) -> Self {
        self.expected = ExpectedOutput::ProgramOutput {
            input: None,
            expected: output,
        };
        self
    }

    /// Modifies the current test runner to expect a compile pass, with the output program
    /// producing the given output when given the following input on stdin.
    pub fn expect_output_with_input(mut self, input: &'static str, output: &'static str) -> Self {
        self.expected = ExpectedOutput::ProgramOutput {
            input: Some(input),
            expected: output,
        };
        self
    }

    /// Executes this test runner. Panics on test failure.
    pub fn run(self) {
        // Create a build configuration based on our inputs.
        // The input file here is mocked and not a real path, but that shouldn't matter.
        let out_dir = PathBuf::from_str("target").unwrap();
        let mut out_file = out_dir.clone();
        out_file.push(format!("{}.out", self.name));
        let build_cfg = BuildConfig {
            input_file: PathBuf::from_str(&self.name).unwrap(),
            out_dir: out_dir.clone(),
            out_file,
            gen_security_features: true,
            opt_level: "none".into(),
            output_ast: false,
            output_ir_regex: None,
        };

        // Make sure our output directory exists.
        if !out_dir.exists() {
            std::fs::create_dir(out_dir.clone())
                .expect("Failed to create output directory for test binary.");
        }

        // Run our build!
        let build_result = crate::commands::build_file(&self.input, &build_cfg);

        // Check the build output matches our expected output.
        match self.expected {
            // No testing required.
            ExpectedOutput::None => return,

            // Test that we failed a compile.
            ExpectedOutput::CompileFailure { reason } => {
                if !build_result.is_err() {
                    panic!(
                        "Test {} expected build to fail, but build succeeded.",
                        self.name
                    );
                }
                if let Some(reason) = reason {
                    let build_output = build_result.unwrap_err().to_string();
                    if !build_output.contains(reason) {
                        panic!("Test {} expected build to fail with reason '{}', but the build did not fail with that reason. Compiler output: {}", self.name, reason, build_output);
                    }
                }
            }

            // Test that our compile succeeded.
            ExpectedOutput::CompilePass => {
                if let Err(e) = build_result {
                    panic!(
                        "Test {} expected to pass, but failed with error: {}",
                        self.name, e
                    );
                }
            }

            // Test that our compile succeeded, and additionally produces some output.
            ExpectedOutput::ProgramOutput { input, expected } => {
                if let Err(e) = build_result {
                    panic!(
                        "Test {} expected to pass, but failed with error: {}",
                        self.name, e
                    );
                }
                Self::test_output(&self.name, input, expected);
            }
        }

        // Remove any remaining object files in the test output directory.
        for path in std::fs::read_dir("target").unwrap() {
            let path = path.unwrap().path();
            if path.to_str().unwrap() == format!("{}.o", self.name)
                || path.to_str().unwrap() == format!("{}.out", self.name)
            {
                let _ = std::fs::remove_file(path);
            }
        }
    }

    /// Tests the output of a single common test runner, assuming that output is placed at
    /// `./target/{test_name}.out`. Panics on failure.
    fn test_output(test_name: &str, input: Option<&str>, expected: &str) {
        let mut out_bin = PathBuf::from_str("target").unwrap();
        out_bin.push(format!("{}.out", test_name));

        // Execute with/without `stdin` and get output.
        let output = if let Some(input) = input {
            run_bin_stdin(&out_bin, input)
        } else {
            run_bin_nostdin(&out_bin)
        };

        // Check if output matches expected.
        if output != expected {
            panic!("Failure for test '{}' output conformancy:\n=== Expected ===\n{}\n=== Found ===\n{}", test_name, expected, output);
        }
    }
}

/// Executes the given binary, returning the output that the command created with no input.
/// Panics on failure to execute.
fn run_bin_nostdin(bin: &PathBuf) -> String {
    let mut cmd = Command::new(bin.to_str().unwrap());
    String::from_utf8(
        cmd.output()
            .expect(&format!(
                "Failed to execute test binary: {}",
                bin.to_str().unwrap()
            ))
            .stdout,
    )
    .unwrap()
}

/// Executes the given binary, passing the provided input via. `stdin`.
/// Returns the output generated by the given program on `stdout`. Panics on failure.
fn run_bin_stdin(bin: &PathBuf, input: &str) -> String {
    let mut cmd = Command::new(bin.to_str().unwrap());
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    let input_bytes = input.as_bytes();

    let mut child = cmd.spawn().expect(&format!(
        "Failed to spawn child test process: {}",
        bin.to_str().unwrap()
    ));
    if let Some(mut child_stdin) = child.stdin.take() {
        child_stdin.write_all(input_bytes).unwrap();
    }
    let stdout = child
        .wait_with_output()
        .expect(&format!(
            "Failed to execute child test process: {}",
            bin.to_str().unwrap()
        ))
        .stdout;
    String::from_utf8(stdout).unwrap()
}
