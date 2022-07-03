{
  description = "futr nostr desktop client";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nix.url = "github:prolic/haskell.nix/patch-2";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  };

  outputs = inputs:
    let
      makeNixpkgs = system:
        let projectOverlay = final: prev: {
          project = final.haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc8107";

            shell.tools = {
              cabal = { };
              ghcid = { };
            };
          };
        };

        cdparanoiaOverlay = final: prev: {
          cdparanoia = prev.cdparanoia.overrideAttrs ( prev: {
            pname = "cdparanoia-III";

            src = final.fetchurl {
              url = "https://downloads.xiph.org/releases/cdparanoia/cdparanoia-III-10.2.src.tgz";
              sha256 = "1pv4zrajm46za0f6lv162iqffih57a8ly4pc69f7y0gfyigb8p80";
            };

            patches = final.lib.optionals final.stdenv.isDarwin [
                (final.fetchpatch {
                  url = "https://trac.macports.org/export/70964/trunk/dports/audio/cdparanoia/files/osx_interface.patch";
                  sha256 = "0hq3lvfr0h1m3p0r33jij0s1aspiqlpy533rwv19zrfllb39qvr8";
                  excludes = [ "configure.in" ];
                })
                (final.fetchurl {
                  url = "https://trac.macports.org/export/70964/trunk/dports/audio/cdparanoia/files/patch-paranoia_paranoia.c.10.4.diff";
                  sha256 = "17l2qhn8sh4jy6ryy5si6ll6dndcm0r537rlmk4a6a8vkn852vad";
                })
              ] ++ [
                ./nix/cdparanoia-patch/fix_private_keyword.patch
                ./nix/cdparanoia-patch/configure.patch
              ] ++ final.lib.optional final.stdenv.hostPlatform.isMusl ./nix/cdparanoia-patch/utils.patch;

            nativeBuildInputs = [
              final.updateAutotoolsGnuConfigScriptsHook
              final.autoreconfHook
            ];
          });
        };

        in
        import inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskell-nix) config;
          overlays = [ inputs.haskell-nix.overlay projectOverlay cdparanoiaOverlay ];
        };

      flake = inputs.flake-utils.lib.eachDefaultSystem (system:
        (makeNixpkgs system).project.flake { });
    in
    inputs.nixpkgs.lib.attrsets.recursiveUpdate flake {
      # Must cross-compile from x86_64-linux only
      packages.x86_64-linux.default =
        (makeNixpkgs "x86_64-linux").pkgsCross.musl64.project.hsPkgs.futr.components.exes.futr;
    };
}
