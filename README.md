# Compression Algorithms in Ada

A collection of compression and transformation algorithms implemented in Ada 2012. This project was done to explore the algorithms and the outcome is not intended to be efficient for real usage. 


## Overview

This project provides standalone command-line tools and library packages for various compression techniques:

### Compression Algorithms
- **Huffman** - Entropy encoding using variable-length codes based on character frequency
- **LZW** - Dictionary-based compression for repeated sequences
- **RLE** - Run-Length Encoding for data with runs of identical bytes

### Transform Algorithms
- **BWT** - Burrows-Wheeler Transform (index-based and sentinel-based versions)
- **MTF** - Move-to-Front Transform

## Requirements

- Ada 2012 compiler (GNAT)
- GNU Make

## Building

```bash
make build    # Build all executables
make clean    # Remove build artifacts
```

Executables are placed in the `bin/` directory.

## Usage

All tools follow a consistent command-line interface:

### Huffman
```bash
./bin/huffman -c input.txt output.huf    # Compress
./bin/huffman -d output.huf restored.txt # Decompress
```

### LZW
```bash
./bin/lzw -c input.txt output.lzw        # Compress
./bin/lzw -d output.lzw restored.txt     # Decompress
```

### RLE
```bash
./bin/rle -c input.txt output.rle        # Compress
./bin/rle -d output.rle restored.txt     # Decompress
```

### BWT (Index-based)
```bash
./bin/bwt -c input.txt output.bwt        # Transform
./bin/bwt -d output.bwt restored.txt     # Inverse transform
```

### BWT (Sentinel-based)
```bash
./bin/bwt_sentinel -c input.txt output.bwt
./bin/bwt_sentinel -d output.bwt restored.txt
```

### MTF
```bash
./bin/mtf -e input.txt output.mtf        # Encode
./bin/mtf -d output.mtf restored.txt     # Decode
```

## Algorithm Comparison

| Algorithm | Type | Best For | Complexity |
|-----------|------|----------|------------|
| Huffman | Compression | General text | O(n log n) |
| LZW | Compression | Repetitive patterns | O(n) |
| RLE | Compression | Long runs of identical bytes | O(n) |
| BWT | Transform | Preprocessing for compression | O(n² log n) |
| MTF | Transform | After BWT, before RLE | O(n × 256) |
for compression
## Compression Pipelines

For maximum compression, algorithms can be chained (similar to bzip2):

```
Input → BWT → MTF → RLE → Huffman → Output
```

BWT groups similar characters together, MTF converts grouped data to sequences of small numbers, RLE compresses runs, and Huffman encodes the resulting skewed distribution.

## Testing

```bash
make test          # Run all tests
make test-huffman  # Run Huffman tests only
make test-lzw      # Run LZW tests only
make test-rle      # Run RLE tests only
make test-mtf      # Run MTF tests only
```

Tests verify perfect round-trip integrity: decompressing a compressed file produces the exact original.

