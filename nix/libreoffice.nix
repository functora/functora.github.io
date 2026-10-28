{pkgs ? import <nixpkgs> {}}:
import ./bubble.nix {
  inherit pkgs;
  name = "libreoffice";
  text = ''
    export XDG_CONFIG_HOME="$HOME/.config"
    ${pkgs.libreoffice}/bin/libreoffice
  '';
  home = "libreoffice";
}
