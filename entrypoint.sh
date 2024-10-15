#!/bin/bash
set -e

case "$1" in
    build)
        echo "Building the project..."
        cabal build all
        ;;
    test)
        echo "Running tests..."
        cabal test
        ;;
    flatpak)
        echo "Building Flatpak..."
        ./build-flatpak.sh
        ;;
    shell)
        echo "Starting shell..."
        /bin/bash
        ;;
    *)
        echo "Usage: $0 {build|test|flatpak|shell}"
        exit 1
esac
