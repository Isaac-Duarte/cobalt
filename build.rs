use std::{fs, path::Path};

/// Build script for copying generated static artifacts from `cobalt_intrinsics` into
/// the relevant target directory with a consistent naming scheme.
/// 
/// This is very hacky, but there doesn't seem to be any standardised way for Rust to
/// generate static library dependencies in a specified location, everything is simply
/// dumped within `target/**` (which isn't useful!).
/// 
/// This *should* update whenever the relevant dependency is changed, but if there's
/// some weird error with intrinsics symbols/behaviour not appearing as expected, try a
/// `cargo clean` and `cargo build`.
fn main() {
    // Retrieve environment variables required for build.
    let man_dir = match std::env::var_os("CARGO_MANIFEST_DIR") {
        None => panic!("No manifest directory found, cannot run build script. Exiting."),
        Some(d) => d,
    };
    let profile = match std::env::var_os("PROFILE") {
        None => panic!("No build profile found, cannot run build script. Exiting."),
        Some(d) => d,
    };

    // Calculate the output path of the `cobalt_intrinsics` library.
    let out_path = Path::new(&man_dir).join("target").join(&profile).join("deps");

    // Re-run this script each time the output directory is altered.
    println!("cargo:rerun-if-changed={}", out_path.to_str().unwrap());
    
    // Search for the appropriate library file.
    println!("{}", out_path.clone().to_str().unwrap());
    let lib_file = fs::read_dir(out_path.clone()).unwrap().map(|entry|
            entry
            .unwrap()
            .path()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string()
        )
        .collect::<Vec<_>>()
        .into_iter()
        .filter(|file_name| file_name.starts_with("libcobalt_intrinsics") && file_name.ends_with(".a"))
        .next();
    match lib_file {
        Some(file) => {
            let orig_path = out_path.join(file);
            let dest_path = Path::new(&man_dir).join("target").join(&profile).join("libcobalt_intrinsics.a");
            fs::copy(orig_path, dest_path).expect("Failed to write output library file.");
        },
        None => panic!("No `libcobalt_intrinsics` library file found within build artifacts.")
    }
}