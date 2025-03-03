{sock}: let
  pkgs = import ./nixpkgs.nix;
  unst = import ./nixpkgs-unstable.nix;
in
  pkgs.writeShellApplication {
    name = "tabby-agent";
    text = ''
      ${pkgs.socat}/bin/socat \
        TCP-LISTEN:8080,fork,reuseaddr,keepalive \
        UNIX:${sock},keepalive &

      SOCAT_PID="$!"

      cleanup() {
        kill "$SOCAT_PID" 2>/dev/null
        exit 0
      }

      trap cleanup SIGINT SIGTERM EXIT

      ${unst.tabby-agent}/bin/tabby-agent "$@"

      cleanup
    '';
  }
