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
        xorg.xev
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
      programs.i3status-rust = {
        enable = true;
        bars.bottom.blocks = [
          {
            block = "battery";
            format = " BAT {percentage} {time}";
            full_format = " BAT: FULL";
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
            format = "SSD{available}";
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
          #
          # Uncomment if swap is used
          #
          #{
          #  block = "memory";
          #  display_type = "swap";
          #  format_swap = " SWP {swap_used_percents}";
          #  clickable = false;
          #  icons_format = "";
          #}
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
      services.screen-locker = {
        enable = true;
        inactiveInterval = 5;
        lockCmd = "${pkgs.i3lock}/bin/i3lock --color=000000";
      };
      xsession.enable = true;
      xsession.windowManager.i3 = {
        enable = true;
        config = let mod = "Mod4"; in {
          modifier = mod;
          keybindings = with pkgs; lib.mkOptionDefault {
            "XF86AudioRaiseVolume" =
              "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +5%";
            "Mod5+Shift+XF86AudioRaiseVolume" =
              "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +5%";

            "XF86AudioLowerVolume" =
              "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -5%";
            "Mod5+Shift+XF86AudioLowerVolume" =
              "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -5%";

            "XF86AudioMute" =
              "exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "Mod5+Shift+XF86AudioMute" =
              "exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle";

            "XF86AudioMicMute" =
              "exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle";
            "Mod5+Shift+XF86AudioMicMute" =
              "exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle";

            "XF86AudioPlay" = "exec ${playerctl}/bin/playerctl play-pause";
            "XF86AudioNext" = "exec ${playerctl}/bin/playerctl next";
            "XF86AudioPrev" = "exec ${playerctl}/bin/playerctl previous";
            "Mod5+Shift+XF86AudioPlay" = "exec ${playerctl}/bin/playerctl play-pause";
            "Mod5+Shift+XF86AudioNext" = "exec ${playerctl}/bin/playerctl next";
            "Mod5+Shift+XF86AudioPrev" = "exec ${playerctl}/bin/playerctl previous";

            "XF86MonBrightnessDown" = "exec \"${brightnessctl}/bin/brightnessctl s 10-\"";
            "XF86MonBrightnessUp" = "exec \"${brightnessctl}/bin/brightnessctl s +10\"";
            "Mod5+Shift+XF86MonBrightnessDown" = "exec \"${brightnessctl}/bin/brightnessctl s 10-\"";
            "Mod5+Shift+XF86MonBrightnessUp" = "exec \"${brightnessctl}/bin/brightnessctl s +10\"";
            "${mod}+h" = "focus left";
            "${mod}+j" = "focus down";
            "${mod}+k" = "focus up";
            "${mod}+l" = "focus right";
            "${mod}+Shift+h" = "move left";
            "${mod}+Shift+j" = "move down";
            "${mod}+Shift+k" = "move up";
            "${mod}+Shift+l" = "move right";
            "Ctrl+Mod1+q" = "exec \"${pkgs.i3lock}/bin/i3lock --color=000000\"";
          };
          bars = [{
            statusCommand =
              "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-bottom.toml";
          }];
          startup = [
            #{
            #  command = "exec i3-msg workspace 1";
            #  always = true;
            #  notification = false;
            #}
            #{
            #  command = "systemctl --user restart polybar.service";
            #  always = true;
            #  notification = false;
            #}
          ];
        };
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
      windowManager.i3.enable = true;
      desktopManager.xterm.enable = false;
      displayManager = {
        defaultSession = "none+i3";
        sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${xkb}";
      };
    };
  };
}

