# Windows Setup Guide

This guide provides instructions for setting up the development environment for the futr nostr client on Windows.

## Automated Builds (Recommended)

The easiest way to get Windows binaries is through our automated CI/CD pipeline using GitHub Actions. No local setup required!

### Continuous Builds
- **Automatic**: Every commit to `master` branch triggers a build
- **Download**: Available at [GitHub Releases](https://github.com/futrnostr/futr/releases/tag/continuous) under "🚧 Development Build (Continuous)"
- **Status**: Check build status in the [Actions tab](https://github.com/futrnostr/futr/actions)
- **Use Case**: Testing latest features and bug fixes

### Stable Releases  
- **Automatic**: Triggered when you push a version tag (e.g., `git tag v1.0.0 && git push origin v1.0.0`)
- **Download**: Available at [GitHub Releases](https://github.com/futrnostr/futr/releases) as stable releases
- **Use Case**: Production use

The automated builds use:
- GHC 9.6.7
- Cabal 3.10.3.0
- MSYS2 MINGW64 environment
- Qt5 with all required modules
- Inno Setup for Windows installer creation

## Manual Development Setup

**When to use manual setup:**
- You want to modify the code and test changes locally
- You need to debug build issues
- You want to contribute to the project
- You prefer building from source

If you just want to use the application, download the automated builds above instead.

### Prerequisites

#### Windows Defender + Antivirus Software

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

### Installation Steps

#### 1. Install GHC + Cabal

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
ghcup install ghc 9.6.7
ghcup install cabal 3.10.3.0
ghcup set ghc 9.6.7
ghcup set cabal 3.10.3.0
```

#### 2. Start the MSYS2 MINGW64 shell

All following commands are from the MSYS2 MINGW64 shell!

**Note**: We use MSYS2 MINGW64 environment for better compatibility and performance. This matches our automated CI/CD setup.

#### 3. Install dependencies (Qt5, LMDB, build tools)

```bash
pacman -Syu --noconfirm
pacman -S --needed --noconfirm git mingw-w64-x86_64-gcc mingw-w64-x86_64-make mingw-w64-x86_64-qt5-base mingw-w64-x86_64-qt5-declarative mingw-w64-x86_64-qt5-graphicaleffects mingw-w64-x86_64-qt5-imageformats mingw-w64-x86_64-qt5-multimedia mingw-w64-x86_64-qt5-quickcontrols2 mingw-w64-x86_64-qt5-svg mingw-w64-x86_64-qt5-tools mingw-w64-x86_64-qt5-translations mingw-w64-x86_64-qt5-winextras mingw-w64-x86_64-openssl mingw-w64-x86_64-angleproject mingw-w64-x86_64-lmdb mingw-w64-x86_64-libunwind mingw-w64-x86_64-toolchain mingw-w64-x86_64-zlib mingw-w64-x86_64-libwebp mingw-w64-x86_64-libjpeg-turbo mingw-w64-x86_64-giflib mingw-w64-x86_64-libwinpthread autoconf autogen automake libtool make
```

#### 4. Setup environment variables

```bash
echo 'export PATH=/usr/bin:/mingw64/bin:/c/tools/ghc-9.6.7/bin:/c/ghcup/bin:$PATH' > ~/setup-env.sh
echo 'export PKG_CONFIG_PATH=C:/msys64/mingw64/lib/pkgconfig:$PKG_CONFIG_PATH' >> ~/setup-env.sh
```

#### 5. Clone HsQML dependency

Clone HsQML parallel to your futr directory:

```bash
# Navigate to your projects directory
cd /c/projects
# Clone HsQML dependency
git clone https://github.com/prolic/HsQML
```

Your directory structure should be similar to this:

```
C:\projects\futr
C:\projects\HsQML
```

#### 6. Build secp256k1

```bash
source ~/setup-env.sh
git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout v0.5.1 && \
    ./autogen.sh && \
    ./configure --prefix=/mingw64 --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental --enable-module-recovery && \
    make && \
    make install && \
    cd ..
```

#### 7. Build the project

```bash
cd futr
source ~/setup-env.sh
cabal build --project-file=cabal.project.windows
```

#### 8. Run the application locally

```bash
source ~/setup-env.sh
cabal run --project-file=cabal.project.windows
```

### Building Windows Installer

#### Prerequisites

Download and install [Inno Setup](https://jrsoftware.org/isdl.php)

#### Steps

1. **Deploy Qt with windeployqt**

```bash
source ~/setup-env.sh
windeployqt.exe dist-newstyle/build/x86_64-windows/ghc-9.6.7/futr-*/build/futr/futr.exe --qmldir=resources/qml
```

2. **Copy additional dependencies**

```bash
bash ./platform/windows/copy-dlls.sh
```

3. **Clean up temporary build artifacts**

```bash
rm -rf dist-newstyle/build/x86_64-windows/ghc-9.6.7/futr-*/build/futr/futr-tmp
```

4. **Create installer binary**

```bash
/c/Program\ Files\ \(x86_64\)/Inno\ Setup\ 6\ISCC.exe platform/windows/innosetup.iss
```

### Troubleshooting

#### pkg-config issues

If you encounter pkg-config errors, verify that the environment variables are set correctly:

```bash
source ~/setup-env.sh
pkg-config --libs libsecp256k1
pkg-config --libs lmdb
```

Both commands should return library paths without errors.

#### Build failures

Make sure you're using the correct project file:

```bash
cabal build --project-file=cabal.project.windows
```

#### Path issues

Ensure the MSYS2 environment variables are loaded in every shell session:

```bash
source ~/setup-env.sh
```
