#!/bin/sh

# Function to display usage information
usage() {
    echo "Usage: $0 <source_file>"
    exit 1
}

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
    usage
fi

SOURCE_FILE=$1
OUTPUT_FILE="output.elf"

# Check if the source file exists
if [ ! -f "$SOURCE_FILE" ]; then
    echo "Error: Source file '$SOURCE_FILE' not found!"
    exit 1
fi

# Compile the program
clang++-16                      \
   --target=riscv64-linux-gnu   \
   -L../../runtime/riscv        \
   -lmlstd                      \
   -lmlrt                       \
   -lffi                        \
   -o "$OUTPUT_FILE"            \
   "$SOURCE_FILE"

# Check if the compilation was successful
if [ $? -eq 0 ]; then
    true
    # echo "Compilation successful! Output: $OUTPUT_FILE"
else
    echo "Compilation failed!"
    exit 1
fi

# Run the compiled program using qemu-riscv64
qemu-riscv64 -L /usr/riscv64-linux-gnu/ -E LD_LIBRARY_PATH=../../runtime/riscv "$OUTPUT_FILE"
