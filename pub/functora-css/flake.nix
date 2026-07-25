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
        serve-functora-css = pkgs.writeShellApplication {
          name = "serve-functora-css";
          text = ''
            ${pkgs.python3}/bin/python3 -m http.server 8000
          '';
        };
        tunnel-functora-css = pkgs.writeShellApplication {
          name = "tunnel-functora-css";
          text = ''
            ${pkgs.cloudflared}/bin/cloudflared tunnel --protocol http2 --edge-ip-version 4 --url http://localhost:8000
          '';
        };
        shell = {
          packages = with pkgs; [
            djlint
            lessc
            clean-css-cli
            serve-functora-css
            tunnel-functora-css
            release-functora-css
            opencode-nix.packages.${system}.default
            chromium
          ];
        };
      in {
        devShells.default = pkgs.mkShell shell;
      }
    );
}
