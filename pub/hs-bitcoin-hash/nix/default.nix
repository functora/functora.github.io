let
  sources = import ./sources.nix;

  ghc-overrides = pkgs: self: super:
    let hs = pkgs.haskell.lib;
    in {
      bitcoin-hash = super.callPackage ../bitcoin-hash/pkg-ghc.nix { };

      _shell = super.shellFor {
        withHoogle = false;
        packages = p: [ p.bitcoin-hash ];
        nativeBuildInputs = [ pkgs.cabal-install ];
      };
    };

  ghcjs-overrides = pkgs: self: super:
    let hs = pkgs.haskell.lib;
    in {
      bitcoin-hash =
        hs.doCheck (super.callPackage ../bitcoin-hash/pkg-ghcjs.nix { });

      extra = hs.dontCheck super.extra;
      quickcheck-assertions = hs.dontCheck super.quickcheck-assertions;
      QuickCheck = hs.dontCheck super.QuickCheck;
      tasty-quickcheck = hs.dontCheck super.tasty-quickcheck;
      terminal-size = super.callCabal2nix "terminal-size" sources.terminal-size {};
      time-compat = hs.dontCheck super.time-compat;

      _shell = super.shellFor {
        withHoogle = false;
        packages = p: [ p.bitcoin-hash ];
        nativeBuildInputs = [ pkgs.nodejs pkgs.cabal-install ];
      };
    };

  pkgs-overlay = self: super: {
    _here = {
      ghc883 = super.haskell.packages.ghc883.override {
        overrides = ghc-overrides self;
      };
      ghc865 = super.haskell.packages.ghc865.override {
        overrides = ghc-overrides self;
      };
      ghcjs86 = super.haskell.packages.ghcjs.override {
        overrides = ghcjs-overrides self;
      };
    };
  };

in import sources.nixpkgs { overlays = [ pkgs-overlay ]; }