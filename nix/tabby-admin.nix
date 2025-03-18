{sock}: let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "tabby-admin";
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

      ${pkgs.vimb}/bin/vimb http://localhost:8080

      cleanup
    '';
  }
