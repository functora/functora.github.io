let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "tabby-socat";
    text = ''
      rm ./tabby.sock || true
      ${pkgs.socat}/bin/socat \
        UNIX-LISTEN:./tabby.sock,fork,reuseaddr,keepalive \
        TCP:localhost:8080,keepalive &
      ${import ./tabby.nix}/bin/tabby "$@" &
      wait -n
      exit $?
    '';
  }
