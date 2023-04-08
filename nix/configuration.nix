{ lib, pkgs, config, ... }:
let
  vi = import ./../pub/vi/nix/default.nix {};
  xkb = pkgs.writeText "xkb-layout" (builtins.readFile ./../cfg/.Xmodmap);
  unst = import ./nixpkgs-unstable.nix;
  yewtube = import ./yewtube.nix {inherit pkgs;};
  lockCmd = "${pkgs.swaylock}/bin/swaylock --color=000000";
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/d01e7280ad7d13a5a0fae57355bd0dbfe5b81969.tar.gz";
    sha256 = "0qh9r3cc8yh434j8n2licf548gvswillzm8x7rfcfys8p7srkvl8";
  };
  kmonad-srv = builtins.fetchTarball {
    url = "https://github.com/kmonad/kmonad/archive/3413f1be996142c8ef4f36e246776a6df7175979.tar.gz";
    sha256 = "0mm439r5qkkpld51spbkmn0qy27sff6iw8c7mb87x73xk4z5cjxq";
  };
  kmonad-src = builtins.fetchTarball {
    url = "https://github.com/kmonad/kmonad/archive/820af08d1ef1bff417829415d5f673041b67ef4d.tar.gz";
    sha256 = "0kkayvcc9jmjm1z1rgabkq36hyrpqdkm8z998dsyg6yh05aqpfzz";
  };
  kmonad-pkg = (import (
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/35bb57c0c8d8b62bbfd284272c928ceb64ddbde9.tar.gz";
      sha256 = "1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2"; }
  ) { src = "${kmonad-src}/nix"; }).defaultNix.default;
  obelisk = import (fetchTarball {
    url = "https://github.com/obsidiansystems/obelisk/archive/41f97410cfa2e22a4ed9e9344abcd58bbe0f3287.tar.gz";
    sha256 = "04bpzji7y3nz573ib3g6icb56s5zbj4zxpakhqaql33v2v77hi9g";
  }){};
  blocked-hosts =
    builtins.concatStringsSep "\n"
      (builtins.map (x: "127.0.0.1 ${x}") [
        "err.ee"
        "delfi.ee"
        "postimees.ee"
        "youtube.com"
        "rumble.com"
        "telegram.org"
        "discord.com"
      ]);
in
{
  imports = [
    (import "${home-manager}/nixos")
    (import "${kmonad-srv}/nix/nixos-module.nix")
  ] ++ (
    if builtins.pathExists ./../prv/nix/configuration.nix
    then [ (import ./../prv/nix/configuration.nix) ]
    else [ ]
  );

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
    #
    # NOTE : enable swap if needed
    #
    # swapDevices = [{
    #   device = "/var/lib/swapfile";
    #   size = 3 * 1024;
    # }];
    #
    # Nix
    #
    nix.settings.cores = 1;
    nix.settings.max-jobs = 1;
    nix.settings.trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    ];
    nix.settings.substituters = [
      "https://cache.iog.io"
      "https://nixcache.reflex-frp.org"
    ];
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    security.polkit.enable = true;
    security.pam.services.swaylock = {};
    #
    # Media
    #
    sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };


    hardware.opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver # LIBVA_DRIVER_NAME=iHD
        vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
        vaapiVdpau
        libvdpau-va-gl
      ];
    };

    services.kmonad = {
      enable = true;
      package = kmonad-pkg;
      # extraArgs = [ "--log-level" "debug" ];
      keyboards.alice80 = {
        device = "/dev/input/by-id/usb-Telink_FEKER_Alice80-event-kbd";
        defcfg = { enable = true; fallthrough = true; allowCommands = false; };
        config = ''
          (defalias
            fst  (layer-toggle fst-layer)
            snd  (layer-toggle snd-layer)
            til  (around lsft grv)
            ltab (around lsft tab)
          )
          (defsrc
            esc  1    2    3    4    5    6              7    8    9    0    -    =    bspc
            tab  q    w    e    r    t              y    u    i    o    p    [    ]    \         del
            caps a    s    d    f    g              h    j    k    l    ;    '    ret            pgup
            lsft z    x    c    v    b              b    n    m    ,    .    /    rsft      up   pgdn
            lctl      lmet      spc       lalt      spc                 rctl           lft  down rght
          )
          (deflayer qwerty
            _    _    _    _    _    _    _              _    _    _    _    _    _    _
            esc  _    _    _    _    _              _    _    _    _    _    _    _    _         _
            @fst _    _    _    _    _              _    _    _    _    _    _    _              _
            _    _    _    _    _    _              _    _    _    _    _    _    ret       _    _
            _         _         _         _         _                   _              _    _    _
          )
          (deflayer fst-layer
            _    f1   f2   f3   f4   f5   f6             f7   f8   f9   f10  f11  f12  _
            @snd _    _    _    _    _              _    _    _    _    _    _    _    grv       _
            _    _    _    slck _    _              lft  down up   rght _    _    _              _
            _    _    _    _    _    _              _    _    _    _    _    _    _         _    _
            _         _         tab       ralt      tab                 _              _    _    _
          )
          (deflayer snd-layer
            _    brdn bru  _    _    _    _              prev pp   next mute vold volu _
            _    _    _    _    _    _              _    _    _    _    _    _    _    @til      _
            _    _    _    _    _    _              home pgdn pgup end  _    _    _              _
            _    _    _    _    _    _              _    _    _    _    _    _    _         _    _
            _         _         @ltab     _         @ltab               _              _    _    _
          )
        '';
      };
      keyboards.gk61 = {
        device = "/dev/input/by-id/usb-SEMITEK_USB-HID_Gaming_Keyboard_SN0000000001-event-kbd";
        defcfg = { enable = true; fallthrough = true; allowCommands = false; };
        config = ''
          (defalias
            fst  (layer-toggle fst-layer)
            snd  (layer-toggle snd-layer)
            trd  (layer-toggle trd-layer)
            til  (around lsft grv)
            ltab (around lsft tab)
          )
          (defsrc
            esc  1    2    3    4    5    6    7    8    9    0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
            caps a    s    d    f    g    h    j    k    l    ;    '    ret
            lsft z    x    c    v    b    n    m    ,    .    /    rsft
            lctl lmet lalt           spc            ralt cmp  rctl
          )
          (deflayer qwerty
            _    _    _    _    _    _    _    _    _    _    _    _    _    _
            esc  _    _    _    _    _    _    _    _    _    _    _    _    _
            @fst _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _    _    _    _    _    _    _    _    _    ret
            _    _    _              _              _    @snd _
          )
          (deflayer fst-layer
            _    f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  _
            @snd _    _    _    _    _    _    _    _    _    _    _    _    grv
            _    _    _    _    _    _    lft  down up   rght _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _              tab            _    _    _
          )
          (deflayer snd-layer
            _    brdn bru  _    _    _    _    prev pp   next mute vold volu _
            _    _    _    _    _    _    _    _    _    _    _    _    _    @til
            _    _    _    _    _    _    home pgdn pgup end  _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _
            _    @trd _              @ltab          _    _    _
          )
          (deflayer trd-layer
            _    _    _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _              _              _    _    _
          )
        '';
      };
    };

    networking.firewall.enable = true;
    virtualisation.docker.enable = false;
    virtualisation.podman.enable = true;
    virtualisation.podman.dockerSocket.enable = true;
    virtualisation.podman.defaultNetwork.dnsname.enable = true;

    users.users.${config.services.functora.userName} = {
      isNormalUser = true;
      description = config.services.functora.userName;
      extraGroups = [ "networkmanager" "wheel" "input" "uinput" "docker" "podman" ];
      packages = with pkgs; [
        firefox
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
        qutebrowser
        xorg.xev
        yewtube
        niv
        unzip
        pciutils
        docker-compose
        htop
        lsof
        wget
        #
        # wayland
        #
        wlr-randr
        swaylock
        swayidle
        wl-clipboard
        mako
        wofi
        waybar
        #
        # development
        #
        obelisk.command
      ];
      programs.git = {
        enable = true;
        userName = "functora";
        userEmail = "functora@proton.me";
      };
      programs.alacritty = {
        enable = true;
        settings = {
          font.size = 14;
          window.startup_mode = "Fullscreen";
        };
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
        ".config/qutebrowser/blocked-hosts".text = blocked-hosts;
        ".config/mps-youtube/config.json".source = ../cfg/yewtube.json;
        ".config/warpd/config".source = ../cfg/warpd.txt;
        ".xkb" = {
          source = ./xkb;
          recursive = true;
        };
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
            icons_format = "";
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
          #
          # NOTE : enable swap if needed
          #
          # {
          #   block = "memory";
          #   display_type = "swap";
          #   format_swap = " SWP {swap_used_percents}";
          #   clickable = false;
          #   icons_format = "";
          # }
          {
            block = "cpu";
            format = " CPU {utilization}";
            icons_format = "";
          }
          {
            block = "sound";
            format = " VOL {volume}";
            show_volume_when_muted = true;
            icons_format = "";
          }
          {
            block = "time";
            format = " %a %Y-%m-%d %R";
            interval = 60;
            icons_format = "";
          }
        ];
      };
      services.udiskie.enable = true;
      services.screen-locker = {
        enable = true;
        inactiveInterval = 5;
        inherit lockCmd;
      };
      services.kanshi = {
        enable = true;
        profiles = {
          docked.outputs = [
            { criteria = "eDP-1"; status = "disable"; }
            { criteria = "HDMI-A-1"; }
          ];
          undocked.outputs = [
            { criteria = "eDP-1"; status = "enable"; }
          ];
        };
      };
      wayland.windowManager.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
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
              wmEx = x:
                "exec --no-startup-id ${x}";
              newBrightness = x:
                wmEx "${brightnessctl}/bin/brightnessctl s ${x}";
              newPlayerCtl = x:
                wmEx "${playerctl}/bin/playerctl ${x}";
              newVolChange = x:
                wmEx "${pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ ${x}";
              cmdVolToggle =
                wmEx "${pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
              cmdMicToggle =
                wmEx "${pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
              newMediaKeys = x: {
                "${x}XF86MonBrightnessDown" = newBrightness "1000-";
                "${x}XF86MonBrightnessUp" = newBrightness "+1000";
                "${x}XF86AudioMicMute" = cmdMicToggle;
                "${x}XF86AudioPrev" = newPlayerCtl "previous";
                "${x}XF86AudioPlay" = newPlayerCtl "play-pause";
                "${x}XF86AudioNext" = newPlayerCtl "next";
                "${x}XF86AudioMute" = cmdVolToggle;
                "${x}XF86AudioLowerVolume" = newVolChange "-5%";
                "${x}XF86AudioRaiseVolume" = newVolChange "+5%";
              };
              cfgProgrKeys = {
                "${mod}+Return" = "exec ${alacritty}/bin/alacritty";
                "${mod}+y" = "exec ${alacritty}/bin/alacritty -e ${yewtube}/bin/yt";
                "${mod}+b" = "exec ${qutebrowser}/bin/qutebrowser";
              };
              cfgBasicKeys = {
                "Ctrl+Mod1+q" = wmEx lockCmd;
                "Ctrl+${mod}+q" = wmEx lockCmd;
                "${mod}+Shift+s" = wmEx "systemctl suspend";
                "${mod}+h" = "focus left";
                "${mod}+j" = "focus down";
                "${mod}+k" = "focus up";
                "${mod}+l" = "focus right";
                "${mod}+Shift+h" = "move left";
                "${mod}+Shift+j" = "move down";
                "${mod}+Shift+k" = "move up";
                "${mod}+Shift+l" = "move right";
                "${mod}+Shift+p" = wmEx "${sway-contrib.grimshot}/bin/grimshot copy output";
                "${mod}+Shift+n" = wmEx "${sway-contrib.grimshot}/bin/grimshot copy area";
                "${mod}+Shift+u" = "exec ${unst.warpd}/bin/warpd --hint";
                "${mod}+Shift+i" = "exec ${unst.warpd}/bin/warpd --normal";
                "${mod}+Shift+y" = "exec ${unst.warpd}/bin/warpd --grid";
                "${mod}+0" = "workspace number 10";
                "${mod}+Shift+0" = "move container to workspace number 10";
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
                  mode = "dock";
                  position = "bottom";
                  trayOutput = "none";
                  statusCommand =
                    "${i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-bottom.toml";
                }];
                seat = { "*" = { hide_cursor = "2000"; }; };
                input = { "*" = {
                  #
                  # NOTE : Moved from xkb to kmonad
                  # for better cross-system compatibility.
                  # Also can add 'compose:ralt' option if
                  # advanced symbol composition is needed.
                  #
                  # xkb_file = "~/.xkb/keymap/custom";
                  xkb_layout = "us,ru";
                  xkb_variant = "altgr-intl,";
                  xkb_options = "grp:sclk_toggle";
                }; };
              };
      };
    };
    #
    # Automount
    #
    services.udisks2.enable = true;
    #
    # XServer
    #
    environment.pathsToLink = ["/libexec"];
    services.xserver = {
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
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = true;
      displayManager.defaultSession = "sway";
      displayManager.sessionPackages = [ pkgs.sway ];
    };
  };
}
