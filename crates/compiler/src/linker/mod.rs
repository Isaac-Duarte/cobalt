/**
 * Structures for linking a final executable from one (or more)
 * compiled object files.
 */
use std::{path::PathBuf, process::Command};

use miette::Result;

use crate::config::BuildConfig;

use self::platform::PlatformConfig;

mod platform;

/// Configures and executes linking of object files produced by the compiler.
pub(crate) struct Linker<'cfg> {
    /// The current build configuration.
    cfg: &'cfg BuildConfig,

    /// A list of user object file paths to link.
    user_objects: Vec<PathBuf>,

    /// Platform-specific linker configuration.
    platform_config: PlatformConfig,
}

impl<'cfg> Linker<'cfg> {
    /// Creates a new linker, automatically detecting the platform-specific config.
    pub fn new(cfg: &'cfg BuildConfig) -> Result<Self> {
        Ok(Linker {
            cfg,
            user_objects: Vec::new(),
            platform_config: PlatformConfig::new()?,
        })
    }

    /// Adds a new user object to be linked.
    /// Order of these objects is important, and parent dependencies must be added first.
    pub fn add_object(&mut self, buf: PathBuf) {
        self.user_objects.push(buf);
    }

    /// Attempts to perform a link with the current set of user objects.
    pub fn link(self) -> Result<()> {
        if self.user_objects.len() == 0 {
            miette::bail!("linker: No user objects specified.");
        }

        // Create initial `ld` command setup, specify output file name.
        let mut ld = Command::new("ld");
        ld.arg("-o").arg(&self.cfg.out_file);

        // Linker files, in order.
        ld.arg(self.platform_config.crt1_o()); // crt1.o
        for user_obj in self.user_objects {
            ld.arg(user_obj);
        }

        // Dynamically linked libraries.
        ld.arg("-lc"); // libc
        ld.arg("-lcobalt_intrinsics"); // libcobalt_intrinsics

        // Add library search path for `cobalt_intrinsics`.
        let mut cur_dir = std::env::current_exe().map_err(|err| {
            miette::diagnostic!("Failed to find path of current executable: {}", err)
        })?;
        cur_dir.pop();
        ld.arg(format!("-L{}", cur_dir.to_str().unwrap()));

        // Additional library search paths.
        for lib_path in self.platform_config.lib_paths() {
            ld.arg(format!("-L{}", lib_path.to_str().unwrap()));
        }

        // Specify the dynamic linker to use (if present).
        match self.platform_config.dyn_linker() {
            Some(dyn_linker) => {
                ld.arg(format!("-dynamic-linker={}", dyn_linker.to_str().unwrap()));
            }
            None => {}
        }

        // Execute the `ld` command.
        let output = ld.output().expect("failed to execute linker");
        if !output.status.success() {
            miette::bail!("linker: {}", String::from_utf8_lossy(&output.stderr));
        }

        Ok(())
    }
}
