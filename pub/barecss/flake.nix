{
  description = "Functora Dev Shell";

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
        release-barecss = pkgs.writeShellApplication {
          name = "release-barecss";
          text = ''
            ${pkgs.lessc}/bin/lessc ./less/bare.less \
              | ${pkgs.clean-css-cli}/bin/cleancss > ./css/bare.min.css
          '';
        };
        shell = {
          packages = with pkgs; [
            djlint
            clean-css-cli
            lessc
            release-barecss
          ];
        };
      in {
        devShells.default = pkgs.mkShell shell;
        devShells.unfree = pkgs.mkShell (shell
          // {
            packages =
              shell.packages
              ++ [
                pkgs.qutebrowser
                pkgs.antigravity
              ];
          });
      }
    );
}
