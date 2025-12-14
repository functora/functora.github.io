{
  description = "Rust Dev Shell";

  inputs = {
    unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    unstable,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = unstable.legacyPackages.${system};
        shell = {
          packages = with pkgs; [
            djlint
            nodejs
            clean-css-cli
            lessc
          ];
        };
      in {
        devShells.default = pkgs.mkShell shell;
      }
    );
}
