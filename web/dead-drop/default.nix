with (import ../../frk/miso/default.nix {}); {
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {miso = miso-jsaddle;};
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
  inherit pkgs;
}
