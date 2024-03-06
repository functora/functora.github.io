{ pkgs ? import (fetchTarball channel:nixos-21.11) {}
}:
{
  ghc865 = pkgs.haskell.packages.ghc865Binary.callPackage ./pkg.nix {};
  ghc884 = pkgs.haskell.packages.ghc884.callPackage ./pkg.nix {};
  ghc8107 = pkgs.haskell.packages.ghc8107.callPackage ./pkg.nix {};
  ghc901 = pkgs.haskell.packages.ghc901.callPackage ./pkg.nix {};
  ghc921 = pkgs.haskell.packages.ghc921.callPackage ./pkg.nix {};
  ghcjs810 = pkgs.haskell.packages.ghcjs810.callPackage ./pkg.nix {};
}
