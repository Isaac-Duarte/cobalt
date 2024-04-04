#!/usr/bin/env bash
set -e

# Builds and runs the Cobalt compiler.
# Author: dev@c272.org

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Run the build script.
. "$SCRIPT_DIR/build.sh"

# Execute the build compiler.
cd "$SCRIPT_DIR"
"$SCRIPT_DIR/build/cobalt" $@