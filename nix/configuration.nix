{ lib, pkgs, config, ... }:
let
  vi = import ./../pkgs/vi/nix/default.nix {};
  xkb = pkgs.writeText "xkb-layout" (builtins.readFile ./../cfg/.Xmodmap);
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/master.tar.gz";
    sha256 = "1ws7acpvz3vp5yzn81ilr5405n29xw9y7hk62d53y6ysqc2yjrk2";
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
    #
    # Misc
    #
    environment.variables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
      BROWSER = "qutebrowser";
      TERMINAL = "alacritty";
    };
    nixpkgs.config.allowUnfree = true;
    #
    # Media
    #
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    hardware.opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver # LIBVA_DRIVER_NAME=iHD
        vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
    #
    # Home
    #
    home-manager.users.${config.services.functora.userName} = {
      home.stateVersion = "22.11";
      home.packages = with pkgs; [
        vi
        tree
        s-tui
        alacritty
        qutebrowser
        (import ./yewtube.nix {inherit pkgs;})
      ];
      programs.git = {
        enable = true;
        userName = "functora";
        userEmail = "functora@proton.me";
      };
      home.file = {
        ".config/qutebrowser/config.py".source = ../cfg/qutebrowser.py;
        ".config/mps-youtube/config.json".source = ../cfg/yewtube.json;
      };
    };
    #
    # XServer
    #
    environment.pathsToLink = ["/libexec"];
    services.xserver = {
      #
      # Keyboard
      #
      layout = "us,ru";
      xkbVariant = "altgr-intl,";
      xkbOptions = "grp:alt_space_toggle";
      #
      # Touchpad
      #
      libinput = {
        enable = true;
        touchpad = {
          tapping = true;
          middleEmulation = true;
          naturalScrolling = true;
        };
      };
      #
      # GUI
      #
      enable = true;
      desktopManager = {
        xterm.enable = false;
      };
      displayManager = {
        defaultSession = "none+i3";
        sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${xkb}";
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

