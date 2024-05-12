#!/usr/bin/env bash
set -e

# Cleans build artifacts from the Cobalt compiler and intrinsics libraries.
# Author: dev@c272.org

###############
## CONSTANTS ##
###############

ORANGE='\033[0;33m'
CYAN='\033[0;36m'
GREEN='\033[1;32m'
NC='\033[0m'

#####################
## BUILD VARIABLES ##
#####################

# The argument for the build profile we're cleaning.
BUILD_PROFILE_ARG=""

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Check if we've got a build profile specified.
if [[ $1 == "dev" ]]; then
    echo -e "${CYAN}Cleaning artifacts for debug mode only.${NC}"
    BUILD_PROFILE_ARG="--profile dev"
fi
if [[ $1 == "release" ]]; then
    echo -e "${CYAN}Cleaning artifacts for release mode only.${NC}"
    BUILD_PROFILE_ARG="--profile release"
fi

# Clean each of the crates in turn.
echo -e "${CYAN}Cleaning compiler artifacts...${NC}"
cd "${SCRIPT_DIR}/crates/compiler/"
cargo clean $BUILD_PROFILE_ARG

echo -e "${CYAN}Cleaning intrinsics library artifacts...${NC}"
cd "${SCRIPT_DIR}/crates/intrinsics/"
cargo clean $BUILD_PROFILE_ARG

echo -e "${CYAN}Cleaning benchmark testbench artifacts...${NC}"
cd "${SCRIPT_DIR}/crates/bench/"
cargo clean $BUILD_PROFILE_ARG

# Clean copied build artifacts in ./build.
echo -e "${CYAN}Cleaning copied build artifacts...${NC}"
BUILD_DIR="${SCRIPT_DIR}/build"
rm -rf "$BUILD_DIR"

# Clean generated benchmark artifacts in ./bench_out.
echo -e "${CYAN}Cleaning generated benchmark artifacts...${NC}"
BENCH_OUT_DIR="${SCRIPT_DIR}/bench_out"
rm -rf $BENCH_OUT_DIR/*.o
rm -rf $BENCH_OUT_DIR/*.c*
rm -rf $BENCH_OUT_DIR/bench_bin

# Done!
echo -e "${GREEN}Clean complete.${NC}"