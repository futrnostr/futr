# Setup Guide

This guide provides instructions for setting up the development environment for the futr nostr client on different operating systems.

## Docker

The easiest way to get started is using the pre-built Docker image.

To build the project, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev build
```

To run the tests, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev test
```

To build the Flatpak, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev flatpak
```

To enter the shell of the container, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev shell
```

### Build the docker image

```bash
docker buildx build . -t prolic/futr-dev
```

## Linux (Ubuntu / Linux Mint)

For all other Linux distros, you can use the Docker image or follow the Ubuntu / Linux Mint instructions below.
You might need to adjust the package names for your distro.

### GHCUp

You can grab GHCUp from the [GHCUp website](https://www.haskell.org/ghcup/).

```bash
sudo apt-get install build-essential zlib1g-dev
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.5
ghcup install cabal 3.10.2.0
ghcup set ghc 9.6.5
ghcup set cabal 3.10.2.0
```

### Qt5 & LMDB

```bash
sudo apt-get install qtdeclarative5-dev \
                     qml-module-qtquick-controls2 \
                     libqt5quick5 \
                     qttools5-dev-tools \
                     qtbase5-dev \
                     qtmultimedia5-dev \
                     qml-module-qtmultimedia \
                     qt5-image-formats-plugins \
                     liblmdb-dev
```

### secp256k1 (from source)

```bash
sudo apt-get install autoconf autogen automake libtool
git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout v0.5.1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental --enable-module-recovery && \
    make && \
    make install && \
    cd ..
```


### Building

```bash
cabal build
```

for Windows:

```bash
cabal build --project-file=cabal.project.windows
```

### Building Flatpak

```bash
cabal build
./build-flatpak.sh
```

## MacOS

@todo

## Windows

### Automated Builds (Recommended)

The easiest way to get Windows binaries is through our automated CI/CD pipeline using AppVeyor. No local setup required!

#### Continuous Builds
- **Automatic**: Every commit to `master` branch triggers a build
- **Download**: Available at [GitHub Releases](https://github.com/prolic/futr/releases/tag/continuous) under "🚧 Development Build (Continuous)"
- **Status**: Check build status on [AppVeyor](https://ci.appveyor.com/project/prolic/futr)
- **Use Case**: Testing latest features and bug fixes

#### Stable Releases  
- **Automatic**: Triggered when you push a version tag (e.g., `git tag v1.0.0 && git push origin v1.0.0`)
- **Download**: Available at [GitHub Releases](https://github.com/prolic/futr/releases) as stable releases
- **Use Case**: Production use

The automated builds use:
- GHC 9.6.5
- Cabal 3.10.2.0
- MINGW64 environment
- Qt5 with all required modules
- Inno Setup for Windows installer creation

### Manual Development Setup

**When to use manual setup:**
- You want to modify the code and test changes locally
- You need to debug build issues
- You want to contribute to the project
- You prefer building from source

If you just want to use the application, download the automated builds above instead.

**Manual setup instructions:**

0. Windows Defender + Antivirus Software

These affect the build time significantly, even on modern hardware. You should consider making exceptions for specific build directories. Here is a PowerShell script that does most of that on Windows Defender for you, adjust paths accordingly.

(Run as administrator)

```powershell
$ghcup = "C:\ghcup"
$cabalStore = "$env:APPDATA\cabal"
$ghcLocal = "$env:LOCALAPPDATA\ghc"
$projectDirs = @(
    "C:\projects\futr",
    "C:\projects\HsQML",
    "C:\projects\libsecp256k1"
)

# Add static exclusions
Add-MpPreference -ExclusionPath $ghcup
Add-MpPreference -ExclusionPath $cabalStore
Add-MpPreference -ExclusionPath $ghcLocal

# Add project-specific exclusions
foreach ($dir in $projectDirs) {
    Add-MpPreference -ExclusionPath $dir
    Add-MpPreference -ExclusionPath (Join-Path $dir "dist-newstyle")
}

Write-Output "Exclusions added successfully."
```

1. Install GHC + Cabal

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
ghcup install ghc 9.6.5
ghcup install cabal 3.10.2.0
ghcup set ghc 9.6.5
ghcup set cabal 3.10.2.0
```

2. Start the MINGW64 shell (should be in "C:\ghcup\msys64\mingw64.exe")

All following commands are from the MINGW64 shell!

**Note**: We use MINGW64 environment for better compatibility and performance. This matches our automated CI/CD setup.

3. Install dependencies (Qt5, LMDB, build tools)

```bash
pacman -Syu
pacman -S autoconf autogen automake libtool gcc git make mingw-w64-x86_64-qt5-base mingw-w64-x86_64-qt5-declarative mingw-w64-x86_64-qt5-graphicaleffects mingw-w64-x86_64-qt5-imageformats mingw-w64-x86_64-qt5-multimedia mingw-w64-x86_64-qt5-quickcontrols2 mingw-w64-x86_64-qt5-svg mingw-w64-x86_64-qt5-tools mingw-w64-x86_64-qt5-translations mingw-w64-x86_64-qt5-winextras mingw-w64-x86_64-openssl mingw-w64-x86_64-angleproject mingw-w64-x86_64-lmdb mingw-w64-x86_64-libunwind mingw-w64-x86_64-toolchain mingw-w64-x86_64-make mingw-w64-x86_64-zlib mingw-w64-x86_64-libc++ mingw-w64-x86_64-libwebp mingw-w64-x86_64-libjpeg-turbo mingw-w64-x86_64-giflib

```

4. secp256k1

```bash
git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout v0.5.1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental --enable-module-recovery && \
    make && \
    make install && \
    cd ..
```

5. Clone HsQML and patch cabal.project

Note: Don't ask me why, just do it. If you can solve it without doing this stupid step, please notify me, I sincerely want to know what is going on.

5.1. Clone HsQML dependency parallel to futr

```bash
# Clone HsQML dependency
git clone https://github.com/prolic/HsQML
```

Your directory structure should be similar to this:

```bash
C:\projects\futr
C:\projects\HsQML
```

Now copy the cabal.project windows fix.

```bash
cd futr
cp cabal.project.windows cabal.project
```

Make sure you don't accidentally commit those changes.

6. Start the build process

```bash
export PATH=/mingw64/bin:$PATH
cabal build
```

7. Run the application locally

```bash
export PATH=/mingw64/bin:$PATH
cabal run
```

8. Build Installer

8.0. Download and install Inno Setup

https://jrsoftware.org/isdl.php

8.1. Deploy Qt with windeployqt

```bash
export PATH=/mingw64/bin:$PATH
windeployqt.exe dist-newstyle/build/x86_64-windows/ghc-9.6.5/futr-0.4.0.0/build/futr/futr.exe --qmldir=resources/qml
```

8.2. Copy further dependencies

```bash
bash ./platform/windows/copy-dlls.sh
```

8.3. Clean up temporary build artifacts

```bash
rm -rf dist-newstyle/build/x86_64-windows/ghc-9.6.5/futr-0.4.0.0/build/futr/futr-tmp
```

8.4. Create Installer Binary

```bash
/c/Program\ Files\ \(x86_64\)/Inno\ Setup\ 6\ISCC.exe platform/windows/innosetup.iss
```

## Profiling

Install ghc-prof-flamegraph if you haven't already:

```bash
cabal install ghc-prof-flamegraph
```

Build and run the program with profiling:

```bash
cabal build --enable-profiling --ghc-options="-fprof-auto -rtsopts"
LC_ALL=C cabal run --enable-profiling futr -- +RTS -p
```

Watch the output:

```bash
less futr.prof
```

Generate the flamegraph:

```bash
ghc-prof-flamegraph futr.prof
```

Open the generated file in your browser:

```bash
xdg-open futr.svg
```
