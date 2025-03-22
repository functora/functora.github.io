let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "hoogle-w3m";
    text = ''
      hoogle serve --port=8081 &

      HOOGLE_PID="$!"

      cleanup() {
        kill "$HOOGLE_PID" 2>/dev/null
        exit 0
      }

      trap cleanup SIGINT SIGTERM EXIT

      ${pkgs.w3m}/bin/w3m -o confirm_qq=0 "http://localhost:8081/?hoogle=$*"

      cleanup
    '';
  }
