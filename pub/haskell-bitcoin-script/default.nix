{ pkgs ? import (import ./nixpkgs.nix) { }, compiler ? "ghc865" }:
pkgs.haskell.packages.${compiler}.callPackage ./bitcoin-script.nix { }
