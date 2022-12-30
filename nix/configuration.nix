{ lib, pkgs, config, ... }:
let
  vi = import ./../pkgs/vi/nix/default.nix {};
  xkb = pkgs.writeText "xkb-layout" (builtins.readFile ./../cfg/.Xmodmap);
  yewtube = import ./yewtube.nix {inherit pkgs;};
  lockCmd = "${pkgs.i3lock}/bin/i3lock --color=000000";
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
    swapDevices = [{
      device = "/var/lib/swapfile";
      size = 4 * 1024;
    }];
    #
    # Nix
    #
    nix.settings.trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    nix.settings.substituters = [
      "https://cache.iog.io"
    ];
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
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
        xorg.xev
        yewtube
        niv
      ];
      programs.git = {
        enable = true;
        userName = "functora";
        userEmail = "functora@proton.me";
      };
      #
      # NOTE : direnv is used to cache nix development shells
      # and protect them from being removed by the garbadge collector
      # https://github.com/nix-community/nix-direnv
      #
      programs.bash.enable = true;
      programs.direnv.enable = true;
      programs.direnv.nix-direnv.enable = true;
      home.file = {
        ".config/qutebrowser/config.py".source = ../cfg/qutebrowser.py;
        ".config/mps-youtube/config.json".source = ../cfg/yewtube.json;
      };
      programs.i3status-rust = {
        enable = true;
        bars.bottom.blocks = [
          {
            block = "battery";
            format = " BAT {percentage} {time}";
            full_format = " BAT FULL";
            icons_format = "";
          }
          {
            block = "backlight";
            format = " BRT {brightness}";
            icons_format = "";
          }
          {
            block = "networkmanager";
          }
          {
            block = "disk_space";
            info_type = "available";
            format = "SSD {available}";
            alert = 10.0;
            warning = 20.0;
            unit = "GB";
            icons_format = "";
          }
          {
            block = "memory";
            display_type = "memory";
            format_mem = " RAM {mem_used_percents}";
            clickable = false;
            icons_format = "";
          }
          {
            block = "memory";
            display_type = "swap";
            format_swap = " SWP {swap_used_percents}";
            clickable = false;
            icons_format = "";
          }
          {
            block = "cpu";
            format = " CPU {utilization}";
            icons_format = "";
          }
          {
            block = "sound";
          }
          {
            block = "time";
            format = " %a %Y-%m-%d %R";
            interval = 60;
            icons_format = "";
          }
        ];
      };
      services.unclutter.enable = true;
      services.gammastep = {
        enable = true;
        provider = "geoclue2";
      };
      services.screen-locker = {
        enable = true;
        inactiveInterval = 5;
        inherit lockCmd;
      };
      xsession.enable = true;
      xsession.windowManager.i3 = {
        enable = true;
        extraConfig = ''
          for_window [class="Alacritty"] fullscreen enable
          for_window [class="qutebrowser"] fullscreen enable
          for_window [class="mpv"] fullscreen enable
          assign [class="qutebrowser"] workspace 10
          assign [class="mpv"] workspace 8
        '';
        config =
          with pkgs;
          let mod =
                "Mod4";
              i3ex = x:
                "exec --no-startup-id ${x}";
              newScreenShot = x:
                i3ex "${maim}/bin/maim ${x} | ${xclip}/bin/xclip -selection clipboard -t image/png";
              newBrightness = x:
                i3ex "${brightnessctl}/bin/brightnessctl s ${x}";
              newPlayerCtl = x:
                i3ex "${playerctl}/bin/playerctl ${x}";
              newVolChange = x:
                i3ex "pactl set-sink-volume @DEFAULT_SINK@ ${x}";
              cmdVolToggle =
                i3ex "pactl set-sink-mute @DEFAULT_SINK@ toggle";
              cmdMicToggle =
                i3ex "pactl set-source-mute @DEFAULT_SOURCE@ toggle";
              newMediaKeys = x: {
                "${x}XF86MonBrightnessDown" = newBrightness "10-";
                "${x}XF86MonBrightnessUp" = newBrightness "+10";
                "${x}XF86AudioMicMute" = cmdMicToggle;
                "${x}XF86AudioPrev" = newPlayerCtl "previous";
                "${x}XF86AudioPlay" = newPlayerCtl "play-pause";
                "${x}XF86AudioNext" = newPlayerCtl "next";
                "${x}XF86AudioMute" = cmdVolToggle;
                "${x}XF86AudioLowerVolume" = newVolChange "-5%";
                "${x}XF86AudioRaiseVolume" = newVolChange "+5%";
              };
              cfgProgrKeys = {
                "${mod}+y" = "exec i3-sensible-terminal -e ${yewtube}/bin/yt";
                "${mod}+q" = "exec ${qutebrowser}/bin/qutebrowser";
              };
              cfgBasicKeys = {
                "Ctrl+Mod1+q" = i3ex lockCmd;
                "${mod}+Shift+s" = i3ex "${lockCmd} && systemctl suspend";
                "${mod}+h" = "focus left";
                "${mod}+j" = "focus down";
                "${mod}+k" = "focus up";
                "${mod}+l" = "focus right";
                "${mod}+Shift+h" = "move left";
                "${mod}+Shift+j" = "move down";
                "${mod}+Shift+k" = "move up";
                "${mod}+Shift+l" = "move right";
                "${mod}+Shift+p" = newScreenShot "";
                "${mod}+Shift+n" = newScreenShot "--select";
              };
          in  {
                modifier = mod;
                defaultWorkspace = "workspace number 1";
                keybindings =
                  lib.mkOptionDefault (
                    newMediaKeys "Mod5+Shift+" //
                    newMediaKeys "" //
                    cfgProgrKeys //
                    cfgBasicKeys
                  );
                bars = [{
                  statusCommand =
                    "${i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-bottom.toml";
                }];
              };
      };
    };
    #
    # XServer
    #
    environment.pathsToLink = ["/libexec"];
    services.geoclue2.enable = true;
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
      windowManager.i3.enable = true;
      desktopManager.xterm.enable = false;
      displayManager = {
        defaultSession = "none+i3";
        sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${xkb}";
      };
    };
  };
}

