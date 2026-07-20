{
  description = "Functora Hakyll Dev Shell";

  inputs = {
    stable.url = "github:nixos/nixpkgs?ref=nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    opencode-nix.url = "github:dominicnunez/opencode-nix";
  };

  outputs = {
    self,
    stable,
    flake-utils,
    opencode-nix,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = stable.legacyPackages.${system};
        hakyll-build = pkgs.writeShellApplication {
          name = "hakyll-build";
          text = ''
            cabal new-run site clean
            sleep 1
            cabal new-run site rebuild
            sleep 2
            cd docs
            htmldoc \
              -f cv.pdf \
              --webpage --header "..." --footer ".1." \
              ./formal.html
          '';
        };
        hakyll-watch = pkgs.writeShellApplication {
          name = "hakyll-watch";
          text = ''
            hakyll-build
            cabal new-run site watch
          '';
        };
        hakyll-release = pkgs.writeShellApplication {
          name = "hakyll-release";
          text = ''
            hakyll-build
            rm -rf ../../docs
            cp -R docs/ ../../docs
          '';
        };
        shell = {
          packages = with pkgs; [
            cabal-install
            ghc
            htmldoc
            haskell-language-server
            zlib
            hakyll-build
            hakyll-watch
            hakyll-release
            opencode-nix.packages.${system}.default
            chromium
            qutebrowser
          ];
        };
      in {
        devShells.default = pkgs.mkShell shell;
      }
    );
}
