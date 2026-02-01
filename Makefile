# Makefile for Compression Algorithms Project
# Copyright (C) 2026 By Ulrik Hørlyk Hjort

PROJECT = compress

# Directories
SRC_DIR = src
BIN_DIR = bin
OBJ_DIR = obj
TEST_DIR = tests

# Compiler
GNATMAKE = gnatmake
GNAT_FLAGS = -gnat2012 -O2
ADA_INCLUDE = -aIsrc/huffman -aIsrc/lzw -aIsrc/rle -aIsrc/bwt -aIsrc/mtf -aIsrc/common
ADA_OBJECTS = -aOobj

.PHONY: all build clean test test-huffman test-lzw test-rle test-bwt test-mtf help

all: build

build:
	@echo "Building compression tools..."
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	@echo "  - Huffman"
	@$(GNATMAKE) $(ADA_INCLUDE) $(ADA_OBJECTS) -o $(BIN_DIR)/huffman src/huffman/huffman_main.adb $(GNAT_FLAGS)
	@echo "  - LZW"
	@$(GNATMAKE) $(ADA_INCLUDE) $(ADA_OBJECTS) -o $(BIN_DIR)/lzw src/lzw/lzw_main.adb $(GNAT_FLAGS)
	@echo "  - RLE"
	@$(GNATMAKE) $(ADA_INCLUDE) $(ADA_OBJECTS) -o $(BIN_DIR)/rle src/rle/rle_main.adb $(GNAT_FLAGS)
	@echo "  - BWT (index-based)"
	@$(GNATMAKE) $(ADA_INCLUDE) $(ADA_OBJECTS) -o $(BIN_DIR)/bwt src/bwt/bwt_main.adb $(GNAT_FLAGS)
	@echo "  - BWT (sentinel-based)"
	@$(GNATMAKE) $(ADA_INCLUDE) $(ADA_OBJECTS) -o $(BIN_DIR)/bwt_sentinel src/bwt/bwt_sentinel_main.adb $(GNAT_FLAGS)
	@echo "  - MTF"
	@$(GNATMAKE) $(ADA_INCLUDE) $(ADA_OBJECTS) -o $(BIN_DIR)/mtf src/mtf/mtf_main.adb $(GNAT_FLAGS)

clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(OBJ_DIR)/* $(BIN_DIR)/* *.ali *.o
	rm -rf $(TEST_DIR)/output

test: build
	@echo "Running Huffman compression tests..."
	@cd tests/huffman && ./run_tests.sh
	@echo ""
	@echo "Running LZW compression tests..."
	@cd tests/lzw && ./run_tests.sh
	@echo ""
	@echo "Running RLE compression tests..."
	@cd tests/rle && ./run_tests.sh
	@echo ""
	@echo "Running MTF encoding tests..."
	@cd tests/mtf && ./run_tests.sh

test-huffman: build
	@echo "Running Huffman compression tests..."
	@cd tests/huffman && ./run_tests.sh

test-lzw: build
	@echo "Running LZW compression tests..."
	@cd tests/lzw && ./run_tests.sh

test-rle: build
	@echo "Running RLE compression tests..."
	@cd tests/rle && ./run_tests.sh

test-mtf: build
	@echo "Running MTF encoding tests..."
	@cd tests/mtf && ./run_tests.sh

help:
	@echo "Compression Algorithms Project - Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all          - Build the project (default)"
	@echo "  build        - Build all compression tools"
	@echo "  clean        - Remove build artifacts"
	@echo "  test         - Run all tests"
	@echo "  test-huffman - Run Huffman tests only"
	@echo "  test-lzw     - Run LZW tests only"
	@echo "  test-rle     - Run RLE tests only"
	@echo "  test-mtf     - Run MTF tests only"
	@echo "  help         - Show this help message"
