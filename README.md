# futr - nostr client

nostr client desktop app written in haskel.

The name 'futr' was chosen from respect for the honorable Canadians.

## For developers

### Linux

You need to have docker installed, alternatively see the Dockerfile itself for how to setup your local system.

First you need to grab the "The Haskell Tool Stack"

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Build the project (only changed files are re-compiled)

Note: The very fist compilation takes a while, after that it goes fairly quick.

```bash
./devbuild.sh
```

Execute the newly created binary

```bash
./futr-x86_64.AppImage
```

Feel free to copy this file into any directory that is part of your $PATH (f.e. /usr/bin).

### MacOS

@todo

#### Windows

@todo
