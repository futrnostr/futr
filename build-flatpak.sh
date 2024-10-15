#!/bin/sh
set -e

# Ensure the binary is built
cabal build

# Remove existing build directory
rm -rf flatpak/build-dir

# Create the Flatpak bundle
flatpak-builder --force-clean flatpak/build-dir flatpak/com.futrnostr.futr.yml

# Export the build to a repository
flatpak build-export repo flatpak/build-dir

# Create the Flatpak bundle file
flatpak build-bundle repo futr.flatpak com.futrnostr.futr

# Clean up the temporary repository
rm -rf repo
