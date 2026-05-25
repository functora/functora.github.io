{
  description = "Functora Dev Shell";

  inputs = {
    stable.url = "github:nixos/nixpkgs?ref=nixos-25.11";
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
            lessc
            clean-css-cli
            release-barecss
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
