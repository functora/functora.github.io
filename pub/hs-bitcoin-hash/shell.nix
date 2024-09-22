let pkgs = import ./nix;
in rec {
  ghc883 = pkgs._here.ghc883._shell;
  ghc865 = pkgs._here.ghc865._shell;
  ghcjs86 = pkgs._here.ghcjs86._shell;
}
