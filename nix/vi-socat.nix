{sock}: let
  pkgs = import ./nixpkgs.nix;
  vi = import ./../pub/vi/nix/default.nix {};
in
  pkgs.writeShellApplication {
    name = "vi";
    text = ''
      ${pkgs.socat}/bin/socat \
        TCP-LISTEN:8080,fork,reuseaddr,keepalive \
        UNIX:${sock},keepalive || true &

      SOCAT_PID="$!"

      cleanup() {
        kill "$SOCAT_PID" 2>/dev/null
        exit 0
      }

      trap cleanup SIGINT SIGTERM EXIT

      ${vi}/bin/vi "$@"

      cleanup
    '';
  }
