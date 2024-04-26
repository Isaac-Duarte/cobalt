#!/usr/bin/env bash
set -e

# Builds the Cobalt compiler and intrinsics libraries, placing the relevant artifacts
# in the `build` directory.
# Author: dev@c272.org

###############
## CONSTANTS ##
###############

RED='\033[0;31m'
ORANGE='\033[0;33m'
CYAN='\033[0;36m'
GREEN='\033[1;32m'
NC='\033[0m'

#####################
## BUILD VARIABLES ##
#####################

# The build profile we're building with.
BUILD_PROFILE="dev"
BUILD_PROFILE_DIR="debug"

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Check if we've got a build mode specified.
if [[ $1 == "release" ]]; then
    echo -e "${CYAN}Building Cobalt in release mode.${NC}"
    BUILD_PROFILE="release"
    BUILD_PROFILE_DIR="release"
fi

# Build each of the crates in turn.
echo -e "${CYAN}Building compiler...${NC}"
cd "${SCRIPT_DIR}/crates/compiler/"
cargo build --profile ${BUILD_PROFILE}

# If the architecture is aarch64, we build with PAC/BTI.
arch=$(uname -m)
echo -e "${CYAN}Building intrinsics library...${NC}"
cd "${SCRIPT_DIR}/crates/intrinsics/"
if [[ $arch == "aarch64" ]]; then
    echo -e "${CYAN}Building intrinsics with aarch64 security features enabled.${NC}"
    rustup default nightly
    rustup component add rust-src --toolchain nightly-aarch64-unknown-linux-gnu
    RUSTFLAGS="-Z branch-protection=bti,pac-ret,leaf" cargo build \
        -Zbuild-std="core,alloc,panic_abort" \
        --target aarch64-unknown-linux-gnu \
        --profile ${BUILD_PROFILE}
    rustup default stable
else
    echo -e "${ORANGE}Non-aarch64 architecture detected, building intrinsics with security features OFF.${NC}"
    cargo build --profile ${BUILD_PROFILE}
fi

# Prepare to copy the build artifacts out.
echo -e "${CYAN}Copying build artifacts...${NC}"
BUILD_DIR="${SCRIPT_DIR}/build"
mkdir -p "$BUILD_DIR"

# Search for the intrinsics artifact.
find . -name libcobalt_intrinsics.a | while read -r line ; do
    if [[ $line =~ "$BUILD_PROFILE_DIR" ]]; then
        cp $line "${BUILD_DIR}"
        break;
    fi
done
if [[ -z "$(find $BUILD_DIR -name libcobalt_intrinsics.a)" ]]; then
    echo -e "${RED}Failed to find intrinsics library artifact.${NC}"
    exit -1
fi

# Copy the compiler executable artifact.
cp "${SCRIPT_DIR}/crates/compiler/target/${BUILD_PROFILE_DIR}/cobalt" "${BUILD_DIR}"

# Done!
echo -e "${GREEN}Build complete.${NC}"