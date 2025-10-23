#!/usr/bin/env bash

echo "Patching pkg-config files..."

# Debug environment
echo "=== Environment Debug ==="
echo "PKG_CONFIG_PATH: $PKG_CONFIG_PATH"
echo "PATH: $PATH"
echo "Current directory: $(pwd)"
echo ""

# Check if pkg-config is available
if ! command -v pkg-config &> /dev/null; then
  echo "Error: pkg-config not found in PATH"
  exit 1
fi

echo "pkg-config version: $(pkg-config --version)"
echo ""

echo "Before patching:"
echo "pkg-config --libs lmdb:"
pkg-config --libs lmdb || echo "Failed to get libs for lmdb"
echo "pkg-config --libs libsecp256k1:"
pkg-config --libs libsecp256k1 || echo "Failed to get libs for libsecp256k1"
echo "pkg-config --cflags lmdb:"
pkg-config --cflags lmdb || echo "Failed to get cflags for lmdb"
echo "pkg-config --cflags libsecp256k1:"
pkg-config --cflags libsecp256k1 || echo "Failed to get cflags for libsecp256k1"
echo ""

# Fix libsecp256k1.pc
LIBSECP256K1_PC="C:/msys64/mingw64/lib/pkgconfig/libsecp256k1.pc"
if [[ -f "$LIBSECP256K1_PC" ]]; then
  echo "Patching $LIBSECP256K1_PC..."
  cp "$LIBSECP256K1_PC" "$LIBSECP256K1_PC.bak"
  
  # Replace prefix and add libdir
  sed -i 's|^prefix=.*|prefix=C:/msys64/mingw64|g' "$LIBSECP256K1_PC"
  
  # Add libdir if it doesn't exist
  if ! grep -q "^libdir=" "$LIBSECP256K1_PC"; then
    echo "libdir=\${prefix}/lib" >> "$LIBSECP256K1_PC"
  else
    sed -i 's|^libdir=.*|libdir=\${prefix}/lib|g' "$LIBSECP256K1_PC"
  fi
  
  echo "Fixed $LIBSECP256K1_PC"
else
  echo "Warning: $LIBSECP256K1_PC not found"
fi

# Fix liblmdb.pc
LIBLMDB_PC="C:/msys64/mingw64/lib/pkgconfig/lmdb.pc"
if [[ -f "$LIBLMDB_PC" ]]; then
  echo "Patching $LIBLMDB_PC..."
  cp "$LIBLMDB_PC" "$LIBLMDB_PC.bak"
  
  # Replace prefix
  sed -i 's|^prefix=.*|prefix=C:/msys64/mingw64|g' "$LIBLMDB_PC"
  
  echo "Fixed $LIBLMDB_PC"
else
  echo "Warning: $LIBLMDB_PC not found"
fi

echo "After patching:"
echo "pkg-config --libs lmdb:"
pkg-config --libs lmdb || echo "Failed to get libs for lmdb"
echo "pkg-config --libs libsecp256k1:"
pkg-config --libs libsecp256k1 || echo "Failed to get libs for libsecp256k1"
echo "pkg-config --cflags lmdb:"
pkg-config --cflags lmdb || echo "Failed to get cflags for lmdb"
echo "pkg-config --cflags libsecp256k1:"
pkg-config --cflags libsecp256k1 || echo "Failed to get cflags for libsecp256k1"
echo ""

echo "Pkg-config files patched successfully!"
