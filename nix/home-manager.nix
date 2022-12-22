{ pkgs }:
{
  home.stateVersion = "22.11";
  home.packages = with pkgs; [

  ];
  programs.git = {
    enable = true;
    userName = "functora";
    userEmail = "functora@proton.me";
  };
}

