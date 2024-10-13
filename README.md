# futr - nostr client

nostr client desktop app written in Haskell and Qt5.

The name 'futr' was chosen out of respect for the honorable Canadians.

## License

Released under GPLv3.

See [License File](LICENSE).

## For developers

### Linux

#### GHCUp

`sudo apt-get install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5`

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.6
ghcup install cabal 3.10.3.0
ghcup set ghc 9.6.6
ghcup set cabal 3.10.3.0
```

#### For QT:

```bash
sudo apt-get install qtdeclarative5-dev && \
                     libqt5quick5 && \
                     qttools5-dev-tools && \
                     qtbase5-dev && \
                     qt5-image-formats-plugins
```

#### For secp256k1 (from source)

`sudo apt-get install autoconf autogen automake libtool`

```bash
git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental --enable-module-recovery && \
    make && \
    make install && \
    cd ..
```

#### For building AppImage

```bash
wget https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage && \
    chmod +x linuxdeploy-x86_64.AppImage                                                                     && \
    sudo mv linuxdeploy-x86_64.AppImage /usr/bin                                                                  && \
    wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage   && \
    chmod +x appimagetool-x86_64.AppImage                                                                    && \
    sudo mv appimagetool-x86_64.AppImage /usr/bin
```

#### Building

```bash
cabal build
```

#### Building AppImage

```bash
cabal build
./build-appimage.sh
```

### MacOS

@todo

#### Windows

```bash
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
ghcup install ghc 9.6.6
ghcup install cabal 3.10.3.0
ghcup set ghc 9.6.6
ghcup set cabal 3.10.3.0
```

@todo

## Profiling

# Install ghc-prof-flamegraph if you haven't already

```bash
cabal install ghc-prof-flamegraph
```

Build and run the program with profiling

```bash
cabal build --enable-profiling --ghc-options="-fprof-auto -rtsopts"
LC_ALL=C cabal run --enable-profiling futr -- +RTS -p
```

Watch the output

```bash
less futr.prof
```

Generate the flamegraph

```bash
ghc-prof-flamegraph futr.prof
```

Open the generated file in your browser

```bash
xdg-open futr.svg
```
