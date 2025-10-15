#!/usr/bin/env bash

CONFIG_FILE="/c/Users/appveyor/AppData/Roaming/cabal/config"

if [[ ! -f "$CONFIG_FILE" ]]; then
  echo "Error: $CONFIG_FILE does not exist."
  exit 1
fi

echo "Patching $CONFIG_FILE to fix UCRT64 build issues..."

# Make a backup
cp "$CONFIG_FILE" "$CONFIG_FILE.bak"

# Use sed to replace paths
#sed -i 's/mingw64/ucrt64/g' "$CONFIG_FILE"

# Comment out extra-include-dirs and extra-lib-dirs to fix __local_stdio_printf_options linker error
# But preserve any clang64 paths that are needed for LMDB and other libraries
sed -i 's/^extra-include-dirs:/-- extra-include-dirs:/g' "$CONFIG_FILE"
sed -i 's/^extra-lib-dirs:/-- extra-lib-dirs:/g' "$CONFIG_FILE"

# Ensure clang64 library paths are available for LMDB and other dependencies
if ! grep -q "C:/msys64/clang64/lib" "$CONFIG_FILE"; then
  echo "Adding clang64 library path for LMDB and other dependencies..."
  echo "extra-lib-dirs: C:/msys64/clang64/lib" >> "$CONFIG_FILE"
fi

echo "Done. Original saved as $CONFIG_FILE.bak"
echo "Commented out extra-include-dirs and extra-lib-dirs to fix UCRT64 linker issues"
