{
  description = "Rust Dev Shell";

  inputs = {
    master.url = "github:nixos/nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
    unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    master,
    nixpkgs,
    unstable,
    rust-overlay,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = unstable.legacyPackages.${system};
        shell = {
          packages = with pkgs; [
            alejandra
            bacon
            cargo
            cargo-edit
            cargo-tarpaulin
            clippy
            rust-analyzer
            rustc
            rustfmt
            wasmtime
            license-generator
          ];
        };
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
          };
        mkRustellNvim = exe:
          pkgs.vimUtils.buildVimPlugin {
            name = "rustell-nvim";
            src = pkgs.writeTextDir "plugin/rustell.vim" ''
              augroup rust.vim.PreWrite
                autocmd!
              augroup END

              augroup rustell_PreWrite
                autocmd!
                autocmd BufWritePre *.rs call s:RustellPreWrite()
              augroup END

              function! s:RustellPreWrite()
                if !filereadable(expand('%'))
                  return
                endif

                " Read buffer
                let l:input = join(getline(1, '$'), "\n")

                " --- run rustell first ---
                let l:rustell_out = system('${exe}', l:input)
                if v:shell_error
                  echohl ErrorMsg | echom 'rustell failed: ' . l:rustell_out | echohl None
                  return
                endif

                " --- now feed result into rustfmt ---
                let l:rustfmt_cmd = 'rustfmt'
                if exists('g:rustfmt_command')
                  let l:rustfmt_cmd = g:rustfmt_command
                endif
                if exists('g:rustfmt_options')
                  let l:rustfmt_cmd .= ' ' . g:rustfmt_options
                endif

                let l:rustfmt_out = system(l:rustfmt_cmd, l:rustell_out)
                if v:shell_error
                  echohl ErrorMsg | echom 'rustfmt failed: ' . l:rustfmt_out | echohl None
                  return
                endif

                " Replace buffer with formatted output
                let l:out_lines = split(l:rustfmt_out, "\n")
                call setline(1, l:out_lines)
                if line('$') > len(l:out_lines)
                  execute len(l:out_lines)+1 . ',$delete _'
                endif
              endfunction
            '';
          };
      in {
        devShells.default = pkgs.mkShell shell;
        devShells.unfree = pkgs.mkShell (shell
          // {
            packages =
              shell.packages
              ++ [
                pkgs.qutebrowser
                master.legacyPackages.${system}.antigravity
              ];
          });
        packages = rec {
          rustell = mkRustPkg "rustell";
          rustell-nvim = mkRustellNvim "${rustell}/bin/rustell";
          rustell-wasm = mkRustPkgWasm "rustell";
          rustell-wasm-nvim = mkRustellNvim "${
            pkgs.wasmtime
          }/bin/wasmtime ${rustell-wasm}/lib/rustell.wasm";
          default = self.packages.${system}.rustell;
        };
      }
    );
}
