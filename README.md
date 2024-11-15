# futr - nostr client

nostr client desktop app written in Haskell and Qt5.

The name 'futr' was chosen out of respect for the honorable Canadians.

## Installation

### Linux

Install via Flatpak:

#### Stable Release

First, import the repository signing key:

```bash
curl -fsSL https://flatpak.futrnostr.com/futr.gpg | gpg --import
```

Then add the repository and install:

```bash
flatpak remote-add --if-not-exists futr-stable https://flatpak.futrnostr.com/futr-stable
flatpak install futr-stable com.futrnostr.futr
```

#### Development Builds

```bash
flatpak remote-add --if-not-exists futr-continuous https://flatpak.futrnostr.com/repo-continuous
flatpak install com.futrnostr.futr
```

### Other Platforms

- Windows - Coming soon
- macOS - Coming soon
- Android - Coming soon
- iOS - Coming soon

## License

Released under GPLv3.

See [License File](LICENSE).

## For developers

For detailed setup instructions, including how to set up the development environment for different operating systems as well as profiling instructions, please refer to the [Setup Guide](docs/Setup.md).
