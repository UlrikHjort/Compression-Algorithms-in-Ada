#!/bin/bash
# ***************************************************************************
#                      MTF Encoding Test Suite
#
#           Copyright (C) 2026 By Ulrik Hørlyk Hjort
# ***************************************************************************

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_DIR="$SCRIPT_DIR/inputs"
OUTPUT_DIR="$SCRIPT_DIR/../output"
MTF_BIN="$SCRIPT_DIR/../../bin/mtf"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "════════════════════════════════════════════════════════════"
echo "  MTF (Move-to-Front) Encoding Test Suite"
echo "════════════════════════════════════════════════════════════"
echo ""

# Check if mtf binary exists
if [ ! -f "$MTF_BIN" ]; then
    echo -e "${RED}✗ Error: MTF binary not found at $MTF_BIN${NC}"
    echo "  Run 'make build' first"
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Clean previous test outputs
rm -f "$OUTPUT_DIR"/*.mtf "$OUTPUT_DIR"/*.out

PASS=0
FAIL=0
TOTAL=0

test_file() {
    local input_file=$1
    local test_name=$2
    
    TOTAL=$((TOTAL + 1))
    
    local basename=$(basename "$input_file")
    local encoded="$OUTPUT_DIR/${basename}.mtf"
    local decoded="$OUTPUT_DIR/${basename}.out"
    
    printf "%-40s " "  $test_name"
    
    # Encode
    if ! "$MTF_BIN" -e "$input_file" "$encoded" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (encoding failed)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Decode
    if ! "$MTF_BIN" -d "$encoded" "$decoded" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (decoding failed)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Verify round-trip
    if ! diff -q "$input_file" "$decoded" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (data mismatch)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Calculate sizes
    local original_size=$(wc -c < "$input_file")
    local encoded_size=$(wc -c < "$encoded")
    
    # Display result  (MTF doesn't compress, just transforms)
    printf "${GREEN}✓ PASS${NC} "
    printf "${BLUE}%6s → %6s (transform)${NC}\n" \
        "$(numfmt --to=iec-i --suffix=B $original_size 2>/dev/null || echo ${original_size}B)" \
        "$(numfmt --to=iec-i --suffix=B $encoded_size 2>/dev/null || echo ${encoded_size}B)"
    
    PASS=$((PASS + 1))
    return 0
}

echo "Running tests from: $INPUT_DIR"
echo ""

# Run all tests
for input_file in "$INPUT_DIR"/*.txt; do
    if [ -f "$input_file" ]; then
        basename=$(basename "$input_file" .txt)
        # Extract test name from filename (remove number prefix)
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
    echo ""
    echo -e "${YELLOW}Note:${NC} MTF is a transform, not compression."
    echo "It prepares data for better compression by other algorithms."
    exit 0
else
    echo -e "${RED}✗ Some tests failed${NC}"
    exit 1
fi
