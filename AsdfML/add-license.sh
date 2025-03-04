#!/usr/bin/env bash

# Define the license header as a single-line string for easier detection
LICENSE_HEADER="(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
"

# Function to format the license in a given file
format_license() {
    local file="$1"
    local temp_file=$(mktemp)

    # Check if the file already starts with the license header
    if head -n $(echo "$LICENSE_HEADER" | wc -l) "$file" | grep -q "Copyright"; then
        # If header already exists, do nothing
        echo "Header already exists in $file"
        rm $temp_file
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

rm "test/many/manytests"

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

ln -s ../../../manytests/ "test/many/manytests"

echo "License formatting complete."
