{pkgs ? import <nixpkgs> {}}:
import ./bubble.nix {
  inherit pkgs;
  name = "prusa";
  text = "${pkgs.prusa-slicer}/bin/prusa-slicer";
  home = "prusa";
}
