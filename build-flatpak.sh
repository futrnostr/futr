#!/bin/sh
set -e

# Ensure the binary is built
cabal build

# Remove existing build directory
rm -rf flatpak/build-dir

# Get version from local git repository
VERSION=$(git describe --tags --abbrev=0 2>/dev/null || echo "dev")
TODAY=$(date +%Y-%m-%d)
APPDATA="flatpak/com.futrnostr.futr.appdata.xml"

# Copy template if appdata.xml doesn't exist
if [ ! -f "$APPDATA" ]; then
  cp flatpak/com.futrnostr.futr.appdata.xml.template "$APPDATA"
fi

# Add new release entry at the top of releases section
sed -i "/<releases>/a \    <release version=\"$VERSION\" date=\"$TODAY\">\n      <description>\n        <p>Release version $VERSION</p>\n      </description>\n    </release>" "$APPDATA"

# Create the Flatpak bundle
flatpak-builder --force-clean flatpak/build-dir flatpak/com.futrnostr.futr.yml

# Export the build to a repository
flatpak build-export repo flatpak/build-dir

# Create the Flatpak bundle file
flatpak build-bundle repo futr.flatpak com.futrnostr.futr

# Clean up the temporary repository
rm -rf repo
