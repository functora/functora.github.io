{
  description = "Functora Dev Shell";

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
        release-functora-css = pkgs.writeShellApplication {
          name = "release-functora-css";
          text = ''
            ${pkgs.lessc}/bin/lessc ./less/functora.less \
              | ${pkgs.clean-css-cli}/bin/cleancss > ./css/functora.min.css
          '';
        };
        shell = {
          packages = with pkgs; [
            djlint
            lessc
            clean-css-cli
            release-functora-css
            opencode-nix.packages.${system}.default
            chromium
            cloudflared
          ];
        };
      in {
        devShells.default = pkgs.mkShell shell;
      }
    );
}
