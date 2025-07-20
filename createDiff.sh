#!/bin/bash

mkdir -p diff

# Get list of changed files (staged and unstaged)
git status --porcelain | awk '{print $2}' | while read -r file; do
  # Ensure parent directories exist
  mkdir -p "diff/$(dirname "$file")"
  # Save the diff
  git diff "$file" > "diff/$file.diff"
done
