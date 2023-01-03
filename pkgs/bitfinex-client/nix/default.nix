let nixpkgs = import ../../nix/nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {
    overlays = import ../../nix/overlay.nix {

    };
  },
}:
with pkgs;

let callPackage = lib.callPackageWith haskellPackages;
    pkg = callPackage ./pkg.nix {inherit lib;};
    systemDeps = [ ];
    testDeps = [ ];
in
  haskell.lib.overrideCabal pkg (drv: {
    setupHaskellDepends =
      if drv ? "setupHaskellDepends"
      then drv.setupHaskellDepends ++ systemDeps
      else systemDeps;
    testSystemDepends =
      if drv ? "testSystemDepends"
      then drv.testSystemDepends ++ testDeps
      else testDeps;
    isExecutable = false;
    enableSharedExecutables = false;
    enableLibraryProfiling = false;
    isLibrary = true;
    doHaddock = false;
    #
    # TODO : test at least public methods
    #
    doCheck = false;
    prePatch = "hpack --force";
  })
