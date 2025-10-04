{
  description = "Rust Dev Shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    rust-overlay,
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
        mkRustPkgWasm = pkg: let
          buildTarget = "wasm32-wasip1";
          pkgs = import nixpkgs {
            inherit system;
            overlays = [rust-overlay.overlays.default];
          };
          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            targets = [buildTarget];
          };
          rustPlatform = pkgs.makeRustPlatform {
            cargo = rustToolchain;
            rustc = rustToolchain;
          };
        in
          rustPlatform.buildRustPackage {
            name = pkg;
            src = pkgs.nix-gitignore.gitignoreSource [] ./${pkg};
            cargoLock.lockFile = ./${pkg}/Cargo.lock;
            buildPhase = ''
              cargo build --release -p ${pkg} --target=${buildTarget}
            '';
            installPhase = ''
              mkdir -p $out/lib
              cp target/${buildTarget}/release/*.wasm $out/lib/
            '';
            # Disable checks if they only work for WASM
            # doCheck = false;
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
            wasmtime
          ];
          shellHook = ''
            SHELL=/run/current-system/sw/bin/bash
          '';
        };
        packages = {
          rustell = mkRustPkg "rustell";
          rustell-wasm = mkRustPkgWasm "rustell";
          default = self.packages.${system}.rustell;
        };
      }
    );
}
