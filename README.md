# futr - nostr client

nostr client desktop app written in haskel.

The name 'futr' was chosen from respect for the honorable Canadians.

## For developers

- You need to follow those guidelines: https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew
- You also need to have secp256k1 with schnorr enabled on your system
- Obviously you need stack

You can use a docker dev-container to simplify your setup:

Build the container:

```bash
docker build -t futr .
```

Install stack:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Build the project (only changed files are re-compiled):

```bash
stack --docker-image=futr build
```

Run the application (automatically builds as well if needed):

```bash
stack --docker-image=futr run
```

Note: The very fist compilation takes a while, after that it goes fairly quick.
