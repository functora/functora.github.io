let
  functora = builtins.fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/2788d8fecd74afe135c90e415b1a2b281fcf4155.tar.gz";
    sha256 = "1yifldba2vkckyxbm7ckr5m2jx867fn16l5paxjq2l6x8m87m27j";
  };
  functora-miso = import "${functora}/frk/miso/default.nix" {
    overlays = import ./overlays.nix {
      inherit functora;
    };
  };
in
  with functora-miso; {
    dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {
      miso = miso-jsaddle;
    };
    release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
    inherit pkgs;
  }
