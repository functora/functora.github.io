{
  description = "Rust Dev Shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        mkRustPkg = pkg:
          pkgs.rustPlatform.buildRustPackage {
            name = pkg;
            src = pkgs.nix-gitignore.gitignoreSource [] ./${pkg};
            cargoLock.lockFile = ./${pkg}/Cargo.lock;
          };
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            alejandra
            bacon
            cargo
            cargo-edit
            clippy
            rust-analyzer
            rustc
            rustfmt
          ];
        };
        packages = {
          rustell = mkRustPkg "rustell";
          default = self.packages.${system}.rustell;
        };
      }
    );
}
