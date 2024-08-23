let pkgs = import ./nix;
in rec {
  ghc883 = pkgs._here.ghc883.bitcoin-keys;
  ghc865 = pkgs._here.ghc865.bitcoin-keys;
  ghcjs86 = pkgs._here.ghcjs86.bitcoin-keys;
}
