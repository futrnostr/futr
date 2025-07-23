#!/usr/bin/env bash

CONFIG_FILE="/c/cabal/config"

if [[ ! -f "$CONFIG_FILE" ]]; then
  echo "Error: $CONFIG_FILE does not exist."
  exit 1
fi

echo "Patching $CONFIG_FILE to use ucrt64 instead of mingw64..."

# Make a backup
cp "$CONFIG_FILE" "$CONFIG_FILE.bak"

# Use sed to replace paths
sed -i 's/mingw64/ucrt64/g' "$CONFIG_FILE"

echo "Done. Original saved as $CONFIG_FILE.bak"
