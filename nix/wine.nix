{pkgs ? import <nixpkgs> {}}:
import ./bubble.nix {
  inherit pkgs;
  name = "wine";
  text = "alacritty";
  runtimeInputs = with pkgs; [
    tmux
    busybox
    alacritty
    wine
    (wine.override {wineBuild = "wine64";})
  ];
  home = "wine";
  mkOverlay = sloth: [
    {
      path = ["bubblewrap" "bind" "ro"];
      update = prev:
        prev
        ++ [
          "/bin/sh"
          (sloth.concat' sloth.homeDir "/.config/tmux")
          (sloth.concat' sloth.homeDir "/.config/alacritty")
        ];
    }
  ];
}
