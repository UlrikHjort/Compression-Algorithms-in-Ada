#!/bin/bash
# ***************************************************************************
#                      BWT Test Suite
#
#           Copyright (C) 2026 By Ulrik Hørlyk Hjort
# ***************************************************************************

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_DIR="$SCRIPT_DIR/inputs"
OUTPUT_DIR="$SCRIPT_DIR/../output"
BWT_BIN="$SCRIPT_DIR/../../bin/bwt"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "════════════════════════════════════════════════════════════"
echo "  BWT (Index-Based) Test Suite"
echo "════════════════════════════════════════════════════════════"
echo ""

if [ ! -f "$BWT_BIN" ]; then
    echo -e "${RED}✗ Error: BWT binary not found at $BWT_BIN${NC}"
    echo "  Run 'make build' first"
    exit 1
fi

mkdir -p "$OUTPUT_DIR"
rm -f "$OUTPUT_DIR"/*.bwt "$OUTPUT_DIR"/*.out

PASS=0
FAIL=0
TOTAL=0

test_file() {
    local input_file=$1
    local test_name=$2
    
    TOTAL=$((TOTAL + 1))
    
    local basename=$(basename "$input_file")
    local transformed="$OUTPUT_DIR/${basename}.bwt"
    local restored="$OUTPUT_DIR/${basename}.out"
    
    printf "%-40s " "  $test_name"
    
    # Transform
    if ! "$BWT_BIN" -t "$input_file" "$transformed" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (transform failed)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Inverse
    if ! "$BWT_BIN" -i "$transformed" "$restored" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (inverse failed)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Verify round-trip
    if ! diff -q "$input_file" "$restored" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (data mismatch)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Calculate sizes
    local original_size=$(wc -c < "$input_file")
    local transformed_size=$(wc -c < "$transformed")
    
    echo -e "${GREEN}✓ PASS${NC} ${BLUE}$original_size → $transformed_size bytes${NC}"
    
    PASS=$((PASS + 1))
    return 0
}

echo "Running tests from: $INPUT_DIR"
echo ""

for input_file in "$INPUT_DIR"/*.txt; do
    if [ -f "$input_file" ]; then
        basename=$(basename "$input_file" .txt)
        test_name=$(echo "$basename" | sed 's/^[0-9]*_//' | tr '_' ' ')
        test_file "$input_file" "$test_name"
    fi
done

echo ""
echo "════════════════════════════════════════════════════════════"
echo "  Results"
echo "════════════════════════════════════════════════════════════"
printf "  Total:  %d\n" $TOTAL
printf "  ${GREEN}Passed: %d${NC}\n" $PASS
printf "  ${RED}Failed: %d${NC}\n" $FAIL
echo ""

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some tests failed${NC}"
    exit 1
fi
