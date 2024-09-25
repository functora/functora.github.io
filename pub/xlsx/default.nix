let
  rev = "07ca3a021f05d6ff46bbd03c418b418abb781279"; # first 21.05 release
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  compiler = "ghc884";
  isLibraryProfiling = false;
  pkgs = import (builtins.fetchTarball url) {
    config = if isLibraryProfiling then ({
      packageOverrides = pkgs_super: {
        haskell = pkgs_super.haskell // {
          packages = pkgs_super.haskell.packages // {
            "${compiler}" = pkgs_super.haskell.packages."${compiler}".override {
              overrides = self: super: {
                mkDerivation = args: super.mkDerivation (args // {
                  enableLibraryProfiling = true;
                });
              };
            };
          };
        };
      };
    }) else {};
  };

  hpkgs = pkgs.haskell.packages."${compiler}";
in pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "xlsx" ./. {}) {
  libraryToolDepends = [pkgs.cabal-install];
}
