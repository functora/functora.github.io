{functora}: [
  (
    _: pkgs: let
      doJailbreak = pkgs.haskell.lib.doJailbreak;
      doCheck = pkgs.haskell.lib.doCheck;
      extendHaskell = ghcpkgs:
        ghcpkgs.extend (
          self: super: {
            #
            # Fixes
            #
            mkDerivation = args:
              super.mkDerivation (args
                // {
                  broken = false;
                  doCheck = false;
                  doHaddock = false;
                  enableLibraryProfiling = false;
                });
            pkcs7 = self.callHackageDirect {
              pkg = "pkcs7";
              ver = "1.0.0.1";
              sha256 = "eeX7kDgzZhhdeA0cWq/UVNxlEPzOO99nUj6qvEmY/yM=";
            } {};
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
            lift-type = self.callHackageDirect {
              pkg = "lift-type";
              ver = "0.1.1.1";
              sha256 = "EIIBNte4+s9s3lthoh1kj/qviuK+zlvBgNpKC0MmHcM=";
            } {};
            uuid = doJailbreak (self.callHackageDirect {
              pkg = "uuid";
              ver = "1.3.11";
              sha256 = "MGfh19sw6wjqknzHLgfDLet1m0pTISe7rsdl/OSwnFM=";
            } {});
            tomland = doJailbreak (self.callHackageDirect {
              pkg = "tomland";
              ver = "1.3.3.2";
              sha256 = "wFarQgkISj3Tq+Wf7AfcT/vowYvOsGhmxp8N66wBLZ4=";
            } {});
            validation-selective = self.callHackageDirect {
              pkg = "validation-selective";
              ver = "0.2.0.0";
              sha256 = "fdaVS5mmemT7aeCOJw67/KplvPShHdGQ2C7pUCjoLco=";
            } {};
            modern-uri = doCheck (doJailbreak (self.callHackageDirect {
              pkg = "modern-uri";
              ver = "0.3.4.4";
              sha256 = "6ppd9FxNMtoZS7878iTFyHdn1fY4kE167PUc1WJ2HKc=";
            } {}));
            megaparsec = doJailbreak (self.callHackageDirect {
              pkg = "megaparsec";
              ver = "9.0.0";
              sha256 = "VHTZlu8Xc8pmrvUk75PLN90q9Ly0ampyJbTEqq9jeA4=";
            } {});
            hspec-megaparsec = doJailbreak (self.callHackageDirect {
              pkg = "hspec-megaparsec";
              ver = "2.2.0";
              sha256 = "zKp8jnhJE5riJSkAKLPds38aoiL3rSs/Cpm8aXWRlDk=";
            } {});
            barbies = self.callHackageDirect {
              pkg = "barbies";
              ver = "2.1.1.0";
              sha256 = "KK6vVyb2a+oiRvNK8eXVJa3lNCPykpelN4XHQCmSxWk=";
            } {};
            optics-core = self.callHackageDirect {
              pkg = "optics-core";
              ver = "0.4.1.1";
              sha256 = "+f0EQFLOZQcQj9ENU9EDJpFIT2qxd3CuQ+6xgIohndg=";
            } {};
            indexed-traversable = self.callHackageDirect {
              pkg = "indexed-traversable";
              ver = "0.1.3";
              sha256 = "TQize782MFwR4ac7Q/mJw/FmwzmDPsZRRPwMUICXjHc=";
            } {};
            foldable1-classes-compat = self.callHackageDirect {
              pkg = "foldable1-classes-compat";
              ver = "0.1";
              sha256 = "Om6/w38G4ZaBZAGzlFb6ElvU4BCU3aOCXogpIZsm4RE=";
            } {};
            #
            # Local
            #
            witch-mini = doJailbreak (
              self.callCabal2nix "witch-mini" "${functora}/pub/witch-mini" {}
            );
            singlethongs =
              self.callCabal2nix
              "singlethongs" "${functora}/pub/singlethongs" {};
            miso =
              self.callCabal2nix
              "miso" "${functora}/ghcjs/miso" {};
            miso-components =
              self.callCabal2nix
              "miso-components" "${functora}/ghcjs/miso-components" {};
            functora-ghcjs =
              self.callCabal2nix
              "functora-ghcjs" "${functora}/pub/functora/src" {};
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
