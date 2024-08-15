let
  # functora = ./..;
  functora = fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/03df5e6d39d0fdc31a53cc350e1125118b4a12ca.tar.gz";
    sha256 = "0pad1981bpnl1szvlhgl00ai9ikz9nsl4yv6mxxv0q46j10bdchz";
  };
  functora-miso = import "${functora}/pub/miso/default.nix" {
    overlays = import ../overlays.nix {inherit functora;};
  };
  pkgs = functora-miso.pkgs;
  doCheck = pkgs.haskell.lib.doCheck;
in rec {
  inherit pkgs functora;
  source = pkgs.nix-gitignore.gitignoreSourcePure ./.gitignore ./.;
  dev = doCheck (pkgs.haskell.packages.ghc865.callCabal2nix "miso-widgets" source {
    miso = functora-miso.miso-jsaddle;
  });
  miso-widgets = pkgs.haskell.packages.ghcjs86.callCabal2nix "miso-widgets" source {};
  vsn = miso-widgets.passthru.version;
}
