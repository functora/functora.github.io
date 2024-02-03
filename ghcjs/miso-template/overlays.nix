{functora}: [
  (
    _: pkgs: let
      doJailbreak = pkgs.haskell.lib.doJailbreak;
      extendHaskell = ghcpkgs:
        ghcpkgs.extend (
          self: super: {
            pkcs7 = doJailbreak (
              self.callHackageDirect {
                pkg = "pkcs7";
                ver = "1.0.0.1";
                sha256 = "eeX7kDgzZhhdeA0cWq/UVNxlEPzOO99nUj6qvEmY/yM=";
              } {}
            );
            qrcode-core = self.callHackageDirect {
              pkg = "qrcode-core";
              ver = "0.9.8";
              sha256 = "exn0OLsSkhcazeeRHIu7Bb+2TEkt4v5R9V2SeTDmhGQ=";
            } {};
            universum = self.callHackageDirect {
              pkg = "universum";
              ver = "1.8.2";
              sha256 = "Ekl/dTUi6nb4GtiG5kFlf+bau8QimRgLZlfticNcbbA=";
            } {};
            witch-mini = doJailbreak (
              self.callCabal2nix "witch-mini" "${functora}/pub/witch-mini" {}
            );
            functora-acme =
              self.callCabal2nix "functora-acme" "${functora}/pub/functora/src" {};
          }
        );
    in {
      haskell =
        pkgs.haskell
        // {
          packages =
            pkgs.haskell.packages
            // {
              ghc865 = extendHaskell pkgs.haskell.packages.ghc865;
              ghcjs86 = extendHaskell pkgs.haskell.packages.ghcjs86;
            };
        };
    }
  )
]
