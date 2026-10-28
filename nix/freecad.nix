{pkgs ? import <nixpkgs> {}}:
import ./bubble.nix {
  inherit pkgs;
  name = "freecad";
  text = "${pkgs.freecad}/bin/freecad";
  home = "prusa";
}
