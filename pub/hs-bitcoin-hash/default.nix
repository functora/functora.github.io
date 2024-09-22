let pkgs = import ./nix;
in rec {
  ghc883 = pkgs._here.ghc883.bitcoin-hash;
  ghc865 = pkgs._here.ghc865.bitcoin-hash;
  ghcjs86 = pkgs._here.ghcjs86.bitcoin-hash;
}
