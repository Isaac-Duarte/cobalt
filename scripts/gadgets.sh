#!/usr/bin/env bash
set -e

# Counts the number of gadgets present in the output binaries from both Cobalt and `cobc`.
# Usage: ./gadgets.sh [COBOL FILE]
# Author: dev@c272.org

###############
## CONSTANTS ##
###############

RED='\033[0;31m'
ORANGE='\033[0;33m'
CYAN='\033[0;36m'
GREEN='\033[1;32m'
NC='\033[0m'

###############
## FUNCTIONS ##
###############

# Prints the usage pattern of this script.
print_usage() {
    echo -e "${RED}Usage: ./gadgets.sh [COBOL-FILE]${NC}"
}

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Check that the number of arguments is valid.
if [ $# -ne 1 ]; then
    echo -e "${RED}Invalid number of arguments.${NC}"
    print_usage
    exit -1
fi

# Build Cobalt in release mode.
cd "$SCRIPT_DIR/.."
./build.sh release

# Create output binaries for each compiler.
cd "$SCRIPT_DIR/.."
./build/cobalt build -O speed_and_size -d ./gadgets_out -o cobalt_hws $1
./build/cobalt build -O speed_and_size -d ./gadgets_out -o cobalt_nohws --disable-security-features $1
cobc -x -free -O3 -o ./gadgets_out/cobc_std $1

# Observe gadget number for each binary.
# For HWS, we also remove any `blr`/`br` gadgets because of BTI.
cd gadgets_out
COBALT_HWS_COUNT=$(\
    ROPgadget --binary cobalt_hws \
        | grep ^0x \
        | sed -E '/autiasp|paciasp/d' \
        | sed -E '/blr|br/d' \
        | wc -l \
)
COBALT_NOHWS_COUNT=$(\
    ROPgadget --binary cobalt_nohws \
        | grep ^0x \
        | sed -E '/autiasp|paciasp/d' \
        | wc -l \
)
COBC_COUNT=$(\
    ROPgadget --binary cobc_std \
        | grep ^0x \
        | sed -E '/autiasp|paciasp/d' \
        | wc -l \
)

# Output results.
echo "cobalt(pac+bti): $COBALT_HWS_COUNT gadgets."
echo "cobalt(none): $COBALT_NOHWS_COUNT gadgets."
echo "cobc(none): $COBC_COUNT gadgets."

# Remove artifacts.
cd "$SCRIPT_DIR/.."
rm -rf gadgets_out