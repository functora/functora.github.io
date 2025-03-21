{functora}: [
  (
    _: pkgs: let
      doJailbreak = pkgs.haskell.lib.doJailbreak;
      doCheck = pkgs.haskell.lib.doCheck;
      extendHaskell = ghcpkgs:
        ghcpkgs.override (old: {
          overrides =
            pkgs.lib.composeExtensions old.overrides
            (
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
                validation-selective = doJailbreak (self.callHackageDirect {
                  pkg = "validation-selective";
                  ver = "0.2.0.0";
                  sha256 = "fdaVS5mmemT7aeCOJw67/KplvPShHdGQ2C7pUCjoLco=";
                } {});
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
                binary-orphans = self.callHackageDirect {
                  pkg = "binary-orphans";
                  ver = "1.0.1";
                  sha256 = "o35lDmSjj2yiU06qQghAtfpnLdBb4QF0Mkp+3t3i4ZY=";
                } {};
                time-compat = self.callHackageDirect {
                  pkg = "time-compat";
                  ver = "1.9.2.2";
                  sha256 = "PmC3gOPpZhwqZ33pFDLYtcV3WcYQxaSvoTBV9AJnbYY=";
                } {};
                aeson = self.callHackageDirect {
                  pkg = "aeson";
                  ver = "1.4.4.0";
                  sha256 = "eXlyMshAGysRuNE7MKbo11cZuNvdzNZ0oy3Y47paWQ4=";
                } {};
                extra = self.callHackageDirect {
                  pkg = "extra";
                  ver = "1.7.16";
                  sha256 = "Eu6I/ZARmObjCfpymyybQDPpakMcqCf3W9gYrT7AR3g=";
                } {};
                bech32 = self.callHackageDirect {
                  pkg = "bech32";
                  ver = "1.1.1";
                  sha256 = "3bzizRrab+AmN3RQoDbbGIr8mc/z0qf7Ki4dsyjD5dM=";
                } {};
                bitcoin-address = self.callHackageDirect {
                  pkg = "bitcoin-address";
                  ver = "0.1";
                  sha256 = "s4v3i6pV02anbYgLqAsbeX3ypzUXhSOqMrkbFxV/jj0=";
                } {};
                bitcoin-hash = self.callHackageDirect {
                  pkg = "bitcoin-hash";
                  ver = "0.1";
                  sha256 = "c18kUghhJB0GjLxHHrgJ8djtNlQXbsJcbnM61zvHwzY=";
                } {};
                bifunctor-classes-compat = self.callHackageDirect {
                  pkg = "bifunctor-classes-compat";
                  ver = "0.1";
                  sha256 = "MkLhIuwjfLomAbbzV3ZI1SWKEKHufFwTcxgCtda4ohI=";
                } {};
                monoid-subclasses = self.callHackageDirect {
                  pkg = "monoid-subclasses";
                  ver = "1.2.5.1";
                  sha256 = "E4MrV6/j5EDC9SdD0uEnViQpm9lrxpofyfp+gjWBQes=";
                } {};
                commutative-semigroups = self.callHackageDirect {
                  pkg = "commutative-semigroups";
                  ver = "0.2.0.1";
                  sha256 = "awswOPDumlMe0AmCb6GgfM2qABp1Y/UxQahMLfj6OEg=";
                } {};
                base-orphans = self.callHackageDirect {
                  pkg = "base-orphans";
                  ver = "0.8.8.2";
                  sha256 = "QbcpILVWIiOa1cc0qhEpd9/wA4vbFmiN1x0FhoXYSB4=";
                } {};
                base16 = doJailbreak (self.callHackageDirect {
                  pkg = "base16";
                  ver = "0.3.2.0";
                  sha256 = "EziiFX/1e105Y7zbO0DLwF6yCzYv4IX/vF9l7B55o7Y=";
                } {});
                network = self.callHackageDirect {
                  pkg = "network";
                  ver = "3.1.2.6";
                  sha256 = "PAnsrKV2vRXJaMQrBFlL2dZQe9i13bEGb/maIjJF2sM=";
                } {};
                socks = self.callHackageDirect {
                  pkg = "socks";
                  ver = "0.6.1";
                  sha256 = "Iqkmih5QFqxPTY2k1IeSXr+VKXwZos86hYAL0Vh6oBw=";
                } {};
                connection = self.callHackageDirect {
                  pkg = "connection";
                  ver = "0.3.1";
                  sha256 = "wMfrtQkkOqtFbz6apiztg6KOKp8Wfs2Otuvr3p34TWI=";
                } {};
                websockets = doJailbreak (self.callHackageDirect {
                  pkg = "websockets";
                  ver = "0.12.7.3";
                  sha256 = "zQY5xQclPNZk7b14ut6Wzcgaolkx+brOxDO5FrZAzk8=";
                } {});
                ghc-source-gen = self.callHackageDirect {
                  pkg = "ghc-source-gen";
                  ver = "0.4.2.0";
                  sha256 = "VWWv8jnXiFxClu5PvQjTY/e29GSZ9NYVYd5BK+XaVa8=";
                } {};
                secp256k1-haskell = self.callHackageDirect {
                  pkg = "secp256k1-haskell";
                  ver = "0.6.1";
                  sha256 = "R4zG6prtWN2bX++hMhVt4VMQoyAVbBZnkMcMA0zFoiQ=";
                } {};
                libsecp256k1 = pkgs.secp256k1;
                #
                # Local
                #
                functora-witch = doJailbreak (
                  self.callCabal2nix
                  "functora-witch"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./../.gitignore
                    ./../pub/functora-witch
                  )
                  {}
                );
                singlethongs =
                  self.callCabal2nix
                  "singlethongs"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./../pub/singlethongs/.gitignore
                    ./../pub/singlethongs
                  )
                  {};
                miso =
                  pkgs.haskell.lib.disableCabalFlag
                  (
                    self.callCabal2nix
                    "miso"
                    (
                      pkgs.nix-gitignore.gitignoreSourcePure
                      ./../pub/miso/.gitignore
                      ./../pub/miso
                    )
                    {}
                  )
                  "logview";
                miso-components =
                  self.callCabal2nix
                  "miso-components"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./miso-components/.gitignore
                    ./miso-components
                  )
                  {};
                miso-widgets =
                  self.callCabal2nix
                  "miso-widgets"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./miso-widgets/.gitignore
                    ./miso-widgets
                  )
                  {};
                miso-functora =
                  self.callCabal2nix
                  "miso-functora"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./miso-functora/.gitignore
                    ./miso-functora
                  )
                  {};
                functora-ghcjs =
                  self.callCabal2nix
                  "functora-ghcjs"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./../.gitignore
                    ./../pub/functora/src
                  )
                  {};
                bitcoin-script =
                  self.callCabal2nix
                  "bitcoin-script"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./../pub/haskell-bitcoin-script/.gitignore
                    ./../pub/haskell-bitcoin-script
                  )
                  {};
                bitcoin-keys =
                  self.callCabal2nix
                  "bitcoin-keys"
                  (
                    pkgs.nix-gitignore.gitignoreSourcePure
                    ./../pub/hs-bitcoin-keys/.gitignore
                    ./../pub/hs-bitcoin-keys/bitcoin-keys
                  )
                  {};
              }
            );
        });
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
