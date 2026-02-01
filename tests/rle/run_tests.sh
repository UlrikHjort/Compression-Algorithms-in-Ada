#!/bin/bash
# ***************************************************************************
#                      RLE Compression Test Suite
#
#           Copyright (C) 2026 By Ulrik Hørlyk Hjort
# ***************************************************************************

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_DIR="$SCRIPT_DIR/inputs"
OUTPUT_DIR="$SCRIPT_DIR/../output"
RLE_BIN="$SCRIPT_DIR/../../bin/rle"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "════════════════════════════════════════════════════════════"
echo "  RLE Compression Test Suite"
echo "════════════════════════════════════════════════════════════"
echo ""

# Check if rle binary exists
if [ ! -f "$RLE_BIN" ]; then
    echo -e "${RED}✗ Error: RLE binary not found at $RLE_BIN${NC}"
    echo "  Run 'make build' first"
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Clean previous test outputs
rm -f "$OUTPUT_DIR"/*.rle "$OUTPUT_DIR"/*.out

PASS=0
FAIL=0
TOTAL=0

test_file() {
    local input_file=$1
    local test_name=$2
    local expected_behavior=$3
    
    TOTAL=$((TOTAL + 1))
    
    local basename=$(basename "$input_file")
    local compressed="$OUTPUT_DIR/${basename}.rle"
    local decompressed="$OUTPUT_DIR/${basename}.out"
    
    printf "%-40s " "  $test_name"
    
    # Compress
    if ! "$RLE_BIN" -c "$input_file" "$compressed" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (compression failed)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Decompress
    if ! "$RLE_BIN" -d "$compressed" "$decompressed" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (decompression failed)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Verify round-trip
    if ! diff -q "$input_file" "$decompressed" > /dev/null 2>&1; then
        echo -e "${RED}✗ FAIL${NC} (data mismatch)"
        FAIL=$((FAIL + 1))
        return 1
    fi
    
    # Calculate compression ratio
    local original_size=$(wc -c < "$input_file")
    local compressed_size=$(wc -c < "$compressed")
    
    if [ $original_size -eq 0 ]; then
        local ratio="N/A"
    else
        local ratio=$((compressed_size * 100 / original_size))
    fi
    
    # Display result
    printf "${GREEN}✓ PASS${NC} "
    printf "${BLUE}%6s → %6s (%3s%%)${NC}\n" \
        "$(numfmt --to=iec-i --suffix=B $original_size 2>/dev/null || echo ${original_size}B)" \
        "$(numfmt --to=iec-i --suffix=B $compressed_size 2>/dev/null || echo ${compressed_size}B)" \
        "$ratio"
    
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
        test_file "$input_file" "$test_name" ""
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
