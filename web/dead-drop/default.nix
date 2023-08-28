let
  functora = builtins.fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/b88c9915705f4387e6ee44936068cb2acc2efefd.tar.gz";
    sha256 = "0hrprgmqzigds9r6wrbw7m4fw5150pvnnak49lmbb4pza214i633";
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
    release = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {};
    inherit pkgs;
  }
