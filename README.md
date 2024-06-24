# futr - nostr client

nostr client desktop app written in haskel.

The name 'futr' was chosen from respect for the honorable Canadians.

## For developers

### Linux

#### GHCUp

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.4.8
ghcup set ghc 9.4.8
```

#### For QT:

`sudo apt-get install qtdeclarative5-dev libqt5quick5`

#### For secp256k1

```bash

RUN git clone https://github.com/bitcoin-core/secp256k1 && \
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
ghcup set ghc 9.4.8
```

@todo
