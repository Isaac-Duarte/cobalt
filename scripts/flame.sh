#!/usr/bin/env bash
set -e

# Creates a flamegraph of the given command, provided the location of the FlameGraph repository.
# Usage: ./flame.sh [FLAMEGRAPH-REPO] -- [COMMAND]
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
    echo -e "${RED}Usage: ./flame.sh [FLAMEGRAPH-REPO] -- [COMMAND]${NC}"
}

################
## ENTRYPOINT ##
################

# Fetch the script directory.
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Check that the number of arguments is valid.
if [ $# -lt 3 ]; then
    echo -e "${RED}Invalid number of arguments.${NC}"
    print_usage
    exit -1
fi

# Check that the provided FlameGraph directory exists.
if [ ! -f "$1/flamegraph.pl" ]; then
    echo -e "${RED}Could not find required scripts in provided FlameGraph directory.${NC}"
    exit -1
fi
FLAMEGRAPH_DIR=$1

# Check that the second argument is a separator.
shift
if [ "$1" != "--" ]; then
    echo -e "${RED}Second argument should always be an argument separator.${NC}"
    print_usage
    exit -1
fi

# Attempt to perform a "perf".
shift
perf record -F 10000 -g -- $@
perf script > out.perf
rm perf.data

# Convert perf data into folded FlameGraph data.
"${FLAMEGRAPH_DIR}/stackcollapse-perf.pl" ./out.perf > out.folded
rm out.perf

# Convert folded FlameGraph data into flamegraph SVG.
"${FLAMEGRAPH_DIR}/flamegraph.pl" ./out.folded > ./flamegraph.svg
rm out.folded