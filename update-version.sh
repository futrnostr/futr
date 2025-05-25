#!/bin/bash

# Script to update version across all files and build the project
# Usage: ./update-version.sh <new-version>

if [ $# -ne 1 ]; then
    echo "Usage: $0 <new-version>"
    echo "Example: $0 0.4.0.0"
    exit 1
fi

NEW_VERSION="$1"

echo "Updating version to: $NEW_VERSION"

# Update version in futr.cabal
sed -i "s/^version:.*$/version:            $NEW_VERSION/" futr.cabal

echo "Updated futr.cabal"

# The Setup.hs will automatically generate all other files from templates during build
echo "Version updated! Run 'cabal build' to generate all platform-specific files."

# Optionally run the build automatically
echo "Running cabal build..."
cabal build

echo "Build complete. All version-dependent files have been generated."
