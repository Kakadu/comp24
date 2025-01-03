#!/bin/bash

# Define the license header as a single-line string for easier detection
LICENSE_HEADER="(** Copyright 2024-2025 KreML Compiler
* SPDX-License-Identifier: LGPL-3.0-or-later *)"

# Function to format the license in a given file
format_license() {
    local file="$1"
    local temp_file=$(mktemp)

    # Check if the file already starts with the license header
    if head -n $(echo "$LICENSE_HEADER" | wc -l) "$file" | grep -q "Copyright 2024-2025"; then
        # If header already exists, do nothing
        echo "Header already exists in $file"
        return
    fi

    # Otherwise, prepend the license header
    {
        echo "$LICENSE_HEADER"
        echo
        cat "$file"
    } > "$temp_file"

    # Replace the original file with the updated file
    mv "$temp_file" "$file"
    echo "Added header to $file"
}

# Recursively process all .ml files in the given directory
process_directory() {
    local dir="$1"
    find "$dir" -type f -name "*.ml*" | while read -r file; do
        format_license "$file"
    done
}

# Run the script with the current directory as the default target
TARGET_DIR=${1:-.}
process_directory "$TARGET_DIR"

echo "License formatting complete."
