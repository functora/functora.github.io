let
  functora = builtins.fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/f1482c538f5a97c2a440d948f45a25953d6c82d1.tar.gz";
    sha256 = "0ij4gnzqr7hsafsr3pqnqihnqlq07hs9pjbgvgmvbas47jhnhdfn";
  };
  functora-miso = import "${functora}/ghcjs/miso/default.nix" {
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
    inherit pkgs functora;
  }
