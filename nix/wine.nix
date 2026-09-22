{
  pkgs ? import <nixpkgs> {},
  user ? "wine",
}:
import ./bubble.nix {
  inherit pkgs user;
  name = "wine";
  text = "alacritty";
  runtimeInputs = with pkgs; [
    tmux
    busybox
    alacritty
    wine
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
