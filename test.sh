#!/usr/bin/env bash
set -e

# Builds and runs the Cobalt unit tests.
# Author: dev@c272.org

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Run the compiler build script in the default debug mode.
. "$SCRIPT_DIR/build.sh"

# Copy the intrinsics artifact into the compiler's target dependencies folder.
# This is a nasty hack to ensure that the unit tests have linkage access to the intrinsics.
cd $SCRIPT_DIR
cp ./build/libcobalt_intrinsics.a ./crates/compiler/target/debug/deps/

# Run the unit tests.
cd "$SCRIPT_DIR/crates/compiler"
cargo test