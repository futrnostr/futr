# Setup Guide

This guide provides instructions for setting up the development environment for the futr nostr client on different operating systems.

## Platform-Specific Setup Guides

For detailed setup instructions, please refer to the platform-specific guides:

- **[Linux Setup Guide](Setup-Linux.md)** - Docker, Ubuntu/Linux Mint, and other Linux distributions
- **[Windows Setup Guide](Setup-Windows.md)** - Automated builds, manual development setup, and installer creation

## Quick Start

### Docker (Linux/macOS)

The easiest way to get started on Linux or macOS is using the pre-built Docker image:

```bash
# Build the project
docker run -it --rm -v $(pwd):/app prolic/futr-dev build

# Run tests
docker run -it --rm -v $(pwd):/app prolic/futr-dev test

# Build Flatpak
docker run -it --rm -v $(pwd):/app prolic/futr-dev flatpak

# Enter container shell
docker run -it --rm -v $(pwd):/app prolic/futr-dev shell
```

### Windows

For Windows, we recommend using our automated builds available at [GitHub Releases](https://github.com/futrnostr/futr/releases):

- **Continuous builds**: Latest development version from `master` branch
- **Stable releases**: Tagged releases for production use

See the [Windows Setup Guide](Setup-Windows.md) for manual development setup instructions.

## Build the Docker Image

```bash
docker buildx build . -t prolic/futr-dev
```

## MacOS

@todo - MacOS setup instructions will be added in a future update.