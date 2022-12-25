{ lib, pkgs, config, ... }:
let
  home-manager =
    builtins.fetchTarball {
      url = "https://github.com/nix-community/home-manager/archive/master.tar.gz";
      sha256 = "1ws7acpvz3vp5yzn81ilr5405n29xw9y7hk62d53y6ysqc2yjrk2";
    };
  home-configs =
    import ./home-manager.nix {
      inherit pkgs;
    };
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

  options.services.functora = with lib; {
    userName = mkOption {
      type = types.str;
    };
  };

  config = {

    environment.variables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
      BROWSER = "qutebrowser";
      TERMINAL = "alacritty";
    };

    home-manager.users.${config.services.functora.userName} = home-configs;

    environment.pathsToLink = ["/libexec"];
    services.xserver = {
      layout = "us";
      xkbVariant = "";

      libinput = {
        enable = true;
        touchpad = {
          tapping = true;
          middleEmulation = true;
          naturalScrolling = true;
        };
      };

      enable = true;
      desktopManager = {
        xterm.enable = false;
      };
      displayManager = {
        defaultSession = "none+i3";
      };
      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          dmenu
          i3status
          i3lock
          i3blocks
        ];
      };
    };
  };
}

