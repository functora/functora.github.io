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
            with-utf8 = self.callHackageDirect {
              pkg = "with-utf8";
              ver = "1.1.0.0";
              sha256 = "yuGp+bHk0Ce5E65z0jP7pCbEW3WIWeSOC2PcnmN/gdg=";
            } {};
            aeson-combinators = self.callHackageDirect {
              pkg = "aeson-combinators";
              ver = "0.1.2.1";
              sha256 = "RSzmDOwyqWv08Dz7XI3ixwO7M+7HDhDxUeOeFLhUI+w=";
            } {};
            generic-lens = self.callHackageDirect {
              pkg = "generic-lens";
              ver = "2.2.2.0";
              sha256 = "TS5v3sWfscKFteAVZfSkv8F7NB5TmE9c2akq6rfo5yM=";
            } {};
            generic-lens-core = self.callHackageDirect {
              pkg = "generic-lens-core";
              ver = "2.2.1.0";
              sha256 = "OgSOJshELndaSSjHeYqtemz5IIfWBZe4VlpL2izq1kY=";
            } {};
            indexed-profunctors = self.callHackageDirect {
              pkg = "indexed-profunctors";
              ver = "0.1.1.1";
              sha256 = "U7fvmUlo/HnOI0OANinj/STCOJapB/DEiPRCJXRdIkE=";
            } {};
            functora-ghcjs =
              self.callCabal2nix "functora-ghcjs" "${functora}/pub/functora/src" {};
            miso-components =
              self.callCabal2nix "miso-components" "${functora}/ghcjs/miso-components" {};
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
