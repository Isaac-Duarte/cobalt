#!/usr/bin/env bash
set -e

# Builds the Cobalt compiler and intrinsics libraries, placing the relevant artifacts
# in the `build` directory.
# Author: dev@c272.org

###############
## CONSTANTS ##
###############

CYAN='\033[0;36m'
GREEN='\033[1;32m'
NC='\033[0m'

#####################
## BUILD VARIABLES ##
#####################

# The build profile we're building with.
PROFILE="debug"

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Build each of the crates in turn.
echo -e "${CYAN}Building compiler...${NC}"
cd "${SCRIPT_DIR}/crates/compiler/"
cargo build

echo -e "${CYAN}Building intrinsics library...${NC}"
cd "${SCRIPT_DIR}/crates/intrinsics/"
cargo build

# Copy the build artifacts out.
echo -e "${CYAN}Copying build artifacts...${NC}"
BUILD_DIR="${SCRIPT_DIR}/build"
mkdir -p "$BUILD_DIR"
cp "${SCRIPT_DIR}/crates/intrinsics/target/${PROFILE}/libcobalt_intrinsics.a" "${BUILD_DIR}"
cp "${SCRIPT_DIR}/crates/compiler/target/${PROFILE}/cobalt" "${BUILD_DIR}"

# Done!
echo -e "${GREEN}Build complete.${NC}"