# Test Suite

This directory contains test files and test runners for all compression algorithms.


## Running Tests

### All Huffman Tests
```bash
cd tests/huffman
./run_tests.sh
```

### From Root (via Makefile)
```bash
make test
```

## Test Input Files

| File | Description | Size | Expected Behavior |
|------|-------------|------|-------------------|
| `01_empty.txt` | Empty file | 0B | Edge case handling |
| `02_single_char.txt` | Single character | 2B | Minimal input |
| `03_repeated_chars.txt` | 1000 repeated 'A' | 1000B | High compression |
| `04_high_entropy.txt` | Random characters | ~70B | Low/no compression |
| `05_large_repetitive.txt` | Large repetitive pattern | ~10KB | High compression |
| `06_real_text.txt` | Real text (README×3) | ~4KB | Typical compression |
| `07_mixed_content.txt` | Mixed patterns | ~250B | Moderate compression |

## Test Output

All test outputs (compressed files, decompressed files) are written to `output/` directory, which is gitignored to keep the repository clean.

### File Extensions

- **`.huf`** - Huffman compressed files (binary format with tree + encoded data)
- **`.out`** - Decompressed output files (used to verify round-trip accuracy)

Example: When testing `01_empty.txt`:
1. Compress: `01_empty.txt` → `01_empty.txt.huf` (compressed)
2. Decompress: `01_empty.txt.huf` → `01_empty.txt.out` (should match original)
3. Verify: `diff 01_empty.txt 01_empty.txt.out` (must be identical)

These files are temporary and automatically cleaned/regenerated on each test run.

## Adding New Tests

1. Create a new input file in `huffman/inputs/` following the naming convention: `NN_description.txt`
2. The test runner will automatically discover and run it
3. Name format: `NN_` = number prefix for ordering, `description` = snake_case description

## Expected Results

- ✓ All round-trips should match original exactly
- ✓ Highly repetitive data: 10-30% of original size
- ✓ Real text: 60-80% of original size
- ✓ High entropy: ~100% or stored uncompressed
- ✓ Empty/small files: May be larger (stored uncompressed)
