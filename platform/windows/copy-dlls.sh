#!/bin/bash

# Script to copy all required DLLs for Windows deployment
# Run this from the futr root directory

# Determine the build directory
BUILD_DIR="dist-newstyle/build/x86_64-windows/ghc-9.6.7/futr-0.3.0.0/build/futr"

if [ ! -d "$BUILD_DIR" ]; then
    echo "Error: Build directory not found: $BUILD_DIR"
    echo "Make sure you've built the project first with 'cabal build'"
    exit 1
fi

echo "Copying DLLs to $BUILD_DIR"

# List of required DLLs from mingw64
DLLS=(
    "libbrotlicommon.dll"
    "libbrotlidec.dll"
    "libbz2-1.dll"
    "libc++.dll"
    "libcrypto-3-x64.dll"
    "libdouble-conversion.dll"
    "libfreetype-6.dll"
    "libgcc_s_seh-1.dll"
    "libglib-2.0-0.dll"
    "libgraphite2.dll"
    "libharfbuzz-0.dll"
    "libiconv-2.dll"
    "libicudt77.dll"
    "libicuin77.dll"
    "libicuuc77.dll"
    "libintl-8.dll"
    "libjpeg-8.dll"
    "liblmdb.dll"
    "libmd4c.dll"
    "libpcre2-16-0.dll"
    "libpcre2-8-0.dll"
    "libpng16-16.dll"
    "libsecp256k1-2.dll"
    "libssl-3-x64.dll"
    "libstdc++-6.dll"
    "libtiff-6.dll"
    "libtiffxx-6.dll"
    "libunwind.dll"
    "libwebp-7.dll"
    "libwinpthread-1.dll"
    "libzstd.dll"
    "zlib1.dll"
)

# Copy each DLL
COPIED=0
MISSING=0

for dll in "${DLLS[@]}"; do
    if [ -f "/mingw64/bin/$dll" ]; then
        cp "/mingw64/bin/$dll" "$BUILD_DIR/"
        echo "✓ Copied $dll"
        ((COPIED++))
    else
        echo "✗ Missing $dll"
        ((MISSING++))
    fi
done

echo ""
echo "Summary:"
echo "  Copied: $COPIED DLLs"
echo "  Missing: $MISSING DLLs"

if [ $MISSING -gt 0 ]; then
    echo ""
    echo "Some DLLs were not found. This might be due to:"
    echo "  1. Different MinGW version"
    echo "  2. Missing packages (install with pacman)"
    echo "  3. Different DLL names in your MinGW installation"
fi
