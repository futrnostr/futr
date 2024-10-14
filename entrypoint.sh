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
    appimage)
        echo "Building AppImage..."
        ./build-appimage.sh
        ;;
    shell)
        echo "Starting shell..."
        /bin/bash
        ;;
    *)
        echo "Usage: $0 {build|test|appimage|shell}"
        exit 1
esac
