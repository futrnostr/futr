# futr - nostr client

nostr client desktop app written in haskel.

The name 'futr' was chosen from respect for the honorable Canadians.

## License

Released under GPLv3.

See [LICENSE](License File).

## For developers

### Linux

#### GHCUp

`sudo apt-get install build-essential`

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.4.8
ghcup install cabal 3.12.1.0
ghcup set ghc 9.4.8
ghcup set cabal 3.12.1.0
```

#### For QT:

`sudo apt-get install qtdeclarative5-dev libqt5quick5`

export LD_LIBRARY_PATH=/home/sasa/Qt/qtdesignstudio-2.3.1-community/qt5_design_studio_reduced_version/lib:$LD_LIBRARY_PATH

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

### MacOS

@todo

#### Windows

```bash
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
ghcup install ghc 9.4.8
ghcup install cabal 3.12.1.0
ghcup set ghc 9.4.8
ghcup set cabal 3.12.1.0
```

@todo
