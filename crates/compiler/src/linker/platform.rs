use std::env;
/**
 * Helpers for platform-specific linker options.
 */
use std::path::PathBuf;
use std::process::Command;

use miette::Result;

/// Platform-specific linker configuration options.
/// Typically auto-detected from the host machine.
pub(super) struct PlatformConfig {
    /// The platform linker to use.
    /// Typically GNU `ld`, however other linkers are supported.
    linker_type: LinkerType,

    /// Location of this platform's relevant `crt1.o` object file.
    crt1_o: PathBuf,

    /// The specific dynamic linker that should be used for this platform, if any.
    dyn_linker: Option<PathBuf>,

    /// Additional library paths that should be searched for this platform config.
    lib_paths: Vec<PathBuf>,
}

/// Represents the type of linker to be used for a given platform.
pub(super) enum LinkerType {
    GnuLd,
    Mold,
}

impl LinkerType {
    /// Returns the binary name for the given linker type.
    pub fn to_binary_name(&self) -> &str {
        match self {
            Self::GnuLd => "ld",
            Self::Mold => "mold",
        }
    }
}

impl PlatformConfig {
    /// Auto-detects the platform linker configuration from the host machine's setup.
    pub(super) fn new() -> Result<Self> {
        if env::consts::ARCH == "x86_64" && env::consts::OS == "linux" {
            Self::detect_linux_x64()
        } else if env::consts::ARCH == "aarch64" && env::consts::OS == "linux" {
            Self::detect_linux_aarch64()
        } else {
            miette::bail!(
                "Unsupported platform/architecture ({}, {}) for linker.",
                env::consts::OS,
                env::consts::ARCH
            );
        }
    }

    pub fn linker_type(&self) -> &LinkerType {
        &self.linker_type
    }

    pub fn crt1_o(&self) -> &PathBuf {
        &self.crt1_o
    }

    pub fn dyn_linker(&self) -> Option<&PathBuf> {
        self.dyn_linker.as_ref()
    }

    pub fn lib_paths(&self) -> &Vec<PathBuf> {
        &self.lib_paths
    }

    /// Detects the linker configuration for an x86_64 Linux host.
    fn detect_linux_x64() -> Result<Self> {
        // Sanity check that the crt, loader exist.
        let crt = PathBuf::from("/usr/lib/x86_64-linux-gnu/crt1.o");
        let loader = PathBuf::from("/lib64/ld-linux-x86-64.so.2");
        if !crt.exists() {
            miette::bail!(
                "linker: Could not find platform `crt1.o` -- file not found @ {}.",
                crt.to_str().unwrap()
            );
        }
        if !loader.exists() {
            miette::bail!(
                "linker: Could not find platform `ld64.so` -- file not found @ {}.",
                loader.to_str().unwrap()
            );
        }

        Ok(PlatformConfig {
            linker_type: Self::detect_best_linker(),
            crt1_o: crt,
            dyn_linker: Some(loader),
            lib_paths: Vec::new(),
        })
    }

    /// Detects the linker configuration for an aarch64 Linux host.
    fn detect_linux_aarch64() -> Result<Self> {
        // Sanity check that the crt, loader exist.
        let crt = PathBuf::from("/usr/lib64/crt1.o");
        let loader = PathBuf::from("/usr/lib/ld-linux-aarch64.so.1");
        if !crt.exists() {
            miette::bail!(
                "linker: Could not find platform `crt1.o` -- file not found @ {}.",
                crt.to_str().unwrap()
            );
        }
        if !loader.exists() {
            miette::bail!(
                "linker: Could not find platform `ld64.so` -- file not found @ {}.",
                loader.to_str().unwrap()
            );
        }

        Ok(PlatformConfig {
            linker_type: Self::detect_best_linker(),
            crt1_o: crt,
            dyn_linker: Some(loader),
            lib_paths: Vec::new(),
        })
    }

    /// Detects the best linker for use on the host system.
    fn detect_best_linker() -> LinkerType {
        // If `mold` is present, use that instead for increased performance.
        if Command::new("mold").spawn().is_ok() {
            LinkerType::Mold
        } else {
            // Not present, fall back to platform linker.
            LinkerType::GnuLd
        }
    }
}
