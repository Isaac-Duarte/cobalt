/**
 * Structures for linking a final executable from one (or more)
 * compiled object files.
 */
use std::{env, path::PathBuf, process::Command};

use miette::Result;

use crate::config::BuildConfig;

use self::platform::{LinkerType, PlatformConfig};

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
            platform_config: PlatformConfig::new(cfg)?,
        })
    }

    /// Adds a new user object to be linked.
    /// Order of these objects is important, and parent dependencies must be added first.
    pub fn add_object(&mut self, buf: PathBuf) {
        self.user_objects.push(buf);
    }

    /// Attempts to perform a link with the current set of user objects.
    pub fn link(self) -> Result<()> {
        if self.user_objects.is_empty() {
            miette::bail!("linker: No user objects specified.");
        }

        // Create initial linker command based on platform.
        let mut ld = Command::new(self.platform_config.linker_type().to_binary_name());

        // If this is mold, we always need to specify a target.
        if let LinkerType::Mold = self.platform_config.linker_type() {
            let mold_target = match env::consts::ARCH {
                "x86_64" => "elf_x86_64",
                "aarch64" => "aarch64linux",
                _ => unreachable!()
            };
            ld.arg(&format!("-m{mold_target}"));
        }
        
        // Specify output executable name.
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

        // If we're using mold, we need to specify "default" search paths too.
        if let LinkerType::Mold = self.platform_config.linker_type() {
            ld.arg("-L/lib/");
            ld.arg("-L/lib64/");
            ld.arg("-L/usr/lib/");
        }

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
        
        // Execute the linker command.
        let output = ld.output().expect("failed to execute linker");
        if !output.status.success() {
            miette::bail!("linker: {}", String::from_utf8_lossy(&output.stderr));
        }

        Ok(())
    }
}
