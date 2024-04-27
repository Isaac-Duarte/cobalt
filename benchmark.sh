#!/usr/bin/env bash
set -e

# Builds and runs the Cobalt compiler benchmarks.
# Author: dev@c272.org

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Run the compiler build script, make sure build is in release mode.
. "$SCRIPT_DIR/build.sh" release

# Run the benchmarks.
cd $SCRIPT_DIR
cargo build --manifest-path="$SCRIPT_DIR/crates/bench/Cargo.toml"
cargo run --manifest-path="$SCRIPT_DIR/crates/bench/Cargo.toml" -- $@