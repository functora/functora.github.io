let
  functora = ../..;
  functora-miso = import "${functora}/ghcjs/miso/default.nix" {};
in
  with functora-miso; {
    inherit pkgs;
    dev = pkgs.haskell.packages.ghc865.callCabal2nix "miso-components" ./. {
      miso = miso-jsaddle;
    };
    app = pkgs.haskell.packages.ghcjs86.callCabal2nix "miso-components" ./. {};
  }
