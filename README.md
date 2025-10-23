# futr - nostr client

[![Linux Tests](https://github.com/futrnostr/futr/workflows/Linux%20Tests/badge.svg)](https://github.com/futrnostr/futr/actions/workflows/linux-tests.yml)
[![Windows Tests](https://github.com/futrnostr/futr/workflows/Windows%20Tests/badge.svg)](https://github.com/futrnostr/futr/actions/workflows/windows-tests.yml)
[![Linux Development Build](https://github.com/futrnostr/futr/workflows/Linux%20Continuous%20Build%20and%20Release/badge.svg)](https://github.com/futrnostr/futr/actions/workflows/linux-continuous.yml)
[![Windows Development Build](https://github.com/futrnostr/futr/workflows/Windows%20Continuous%20Build%20and%20Release/badge.svg)](https://github.com/futrnostr/futr/actions/workflows/windows-continuous.yml)
[![Linux Release](https://github.com/futrnostr/futr/workflows/Linux%20Tagged%20Release/badge.svg)](https://github.com/futrnostr/futr/actions/workflows/linux-release.yml)
[![Windows Release](https://github.com/futrnostr/futr/workflows/Windows%20Tagged%20Release/badge.svg)](https://github.com/futrnostr/futr/actions/workflows/windows-release.yml)

nostr client desktop app written in Haskell and Qt5.

The name 'futr' was chosen out of respect for the honorable Canadians.

## Installation

### Linux

Install via Flatpak:

#### Stable Release

Then add the repository and install:

```bash
flatpak remote-add --if-not-exists futr-stable https://flatpak.futrnostr.com/futr-stable.flatpakrepo
flatpak install com.futrnostr.futr
```

#### Development Builds

```bash
flatpak remote-add --if-not-exists futr-continuous https://flatpak.futrnostr.com/futr-continuous.flatpakrepo
flatpak install com.futrnostr.futr
```

### Windows

Download Windows installers from [GitHub Releases](https://github.com/futrnostr/futr/releases):

- **Stable releases**: Download from the latest stable release
- **Development builds**: Download from [continuous release](https://github.com/futrnostr/futr/releases/tag/continuous) (automatically updated from master branch)

### Other Platforms

- macOS - Coming soon
- Android - Coming soon
- iOS - Coming soon

## License

Released under GPLv3.

See [License File](LICENSE).

## For developers

For detailed setup instructions, including how to set up the development environment for different operating systems as well as profiling instructions, please refer to the [Setup Guide](docs/Setup.md).
