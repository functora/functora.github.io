{ pkgs }:
let vi = import ./../pkgs/vi/nix/default.nix {};
in
{
  home.stateVersion = "22.11";
  home.packages = with pkgs; [
    vi
    tree
    s-tui
    alacritty
    qutebrowser
  ];
  programs.git = {
    enable = true;
    userName = "functora";
    userEmail = "functora@proton.me";
  };
  home.file = {
    ".config/qutebrowser/config.py".source = ../cfg/qutebrowser.py;
  };
}

