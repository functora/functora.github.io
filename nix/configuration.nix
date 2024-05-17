{
  lib,
  pkgs,
  config,
  ...
}: let
  vi = import ./../pub/vi/nix/default.nix {};
  # xkb = pkgs.writeText "xkb-layout" (builtins.readFile ./../cfg/.Xmodmap);
  yewtube = import ./yewtube.nix;
  qmk-setup = import ./qmk-setup.nix;
  lockCmd = "${pkgs.swaylock}/bin/swaylock --color=000000";
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/aeb2232d7a32530d3448318790534d196bf9427a.tar.gz";
    sha256 = "16078fwcmqq41dqfnm124xxm8l6zykvqlj1kzgi0fvfil4y86slm";
  };
  nixos-hardware = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixos-hardware/archive/fa194fc484fd7270ab324bb985593f71102e84d1.tar.gz";
    sha256 = "06yn179lbhql3vkk4cjca4mdwr6lfdh6n1vqma3a4266dap6hcf4";
  };
  kmonad-srv = builtins.fetchTarball {
    url = "https://github.com/kmonad/kmonad/archive/3413f1be996142c8ef4f36e246776a6df7175979.tar.gz";
    sha256 = "0mm439r5qkkpld51spbkmn0qy27sff6iw8c7mb87x73xk4z5cjxq";
  };
  kmonad-src = builtins.fetchTarball {
    url = "https://github.com/kmonad/kmonad/archive/820af08d1ef1bff417829415d5f673041b67ef4d.tar.gz";
    sha256 = "0kkayvcc9jmjm1z1rgabkq36hyrpqdkm8z998dsyg6yh05aqpfzz";
  };
  kmonad-pkg =
    (import (
      fetchTarball {
        url = "https://github.com/edolstra/flake-compat/archive/35bb57c0c8d8b62bbfd284272c928ceb64ddbde9.tar.gz";
        sha256 = "1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2";
      }
    ) {src = "${kmonad-src}/nix";})
    .defaultNix
    .default;
  obelisk = import (fetchTarball {
    url = "https://github.com/obsidiansystems/obelisk/archive/41f97410cfa2e22a4ed9e9344abcd58bbe0f3287.tar.gz";
    sha256 = "04bpzji7y3nz573ib3g6icb56s5zbj4zxpakhqaql33v2v77hi9g";
  }) {};
  blocked-hosts =
    builtins.concatStringsSep "\n"
    (builtins.map (x: "127.0.0.1 ${x} www.${x} www2.${x} web.${x} rus.${x} news.${x}")
      (
        if config.services.functora.blockHosts
        then [
          "err.ee"
          "delfi.ee"
          "postimees.ee"
          "rumble.com"
          "t.me"
          "twitter.com"
          "twitch.tv"
          "tiendamia.com"
          "mercadolibre.com.uy"
          "facebook.com"
          "discord.com"
          "telegram.org"
          "youtube.com"
          # "odysee.com"
          # "bastyon.com"
          # "bitchute.com"
        ]
        else []
      ));
  # bash script to let dbus know about important env variables and
  # propagate them to relevent services run at the end of sway config
  # see
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # note: this is pretty much the same as  /etc/sway/config.d/nixos.conf but also restarts
  # some user services to make sure they have the correct environment variables
  dbus-sway-environment = pkgs.writeShellApplication {
    name = "dbus-sway-environment";
    text = ''
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
      systemctl --user stop pipewire xdg-desktop-portal xdg-desktop-portal-wlr
      systemctl --user start pipewire xdg-desktop-portal xdg-desktop-portal-wlr
    '';
  };
  # currently, there is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # the suggested way to set gtk settings is with gsettings
  # for gsettings to work, we need to tell it where the schemas are
  # using the XDG_DATA_DIR environment variable
  # run at the end of sway config
  configure-gtk = pkgs.writeShellApplication {
    name = "configure-gtk";
    text = let
      schema = pkgs.gsettings-desktop-schemas;
      datadir = "${schema}/share/gsettings-schemas/${schema.name}";
    in ''
      export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
      gnome_schema=org.gnome.desktop.interface
      ${pkgs.glib}/bin/gsettings set $gnome_schema gtk-theme 'Dracula'
    '';
  };
  mkKbd = cfg: dev: {
    config = cfg;
    device = dev;
    defcfg = {
      enable = true;
      fallthrough = true;
      allowCommands = false;
    };
  };
  mk60p = mkKbd ''
    (defalias
      fst  (layer-toggle fst-layer)
      snd  (layer-toggle snd-layer)
      til  (around lsft grv)
      ltab (around lsft tab)
    )
    (defsrc
      esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12
      grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
      caps a    s    d    f    g    h    j    k    l    ;    '    ret
      lsft z    x    c    v    b    n    m    ,    .    /    rsft
      lctl lmet lalt           spc            ralt rmet cmp  rctl
    )
    (deflayer qwerty
      _    _    _    _    _    _    _    _    _    _    _    _    _
      esc  _    _    _    _    _    _    _    _    _    _    _    _    _
      esc  _    _    _    _    _    _    _    _    _    _    _    _    _
      @fst _    _    _    _    _    _    _    _    _    _    _    _
      _    _    _    _    _    _    _    _    _    _    _    ret
      _    _    _              _              _    _    _    _
    )
    (deflayer fst-layer
      _    _    _    _    _    _    _    _    _    _    _    _    _
      _    f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  _
      @snd _    _    _    _    _    _    _    _    _    _    _    _    grv
      _    _    _    slck _    _    lft  down up   rght _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _
      _    _    ralt           tab            _    _    _    _
    )
    (deflayer snd-layer
      _    _    _    _    _    _    _    _    _    _    _    _    _
      _    brdn bru  _    _    _    _    prev pp   next mute vold volu _
      _    _    _    _    _    _    _    _    _    _    _    _    _    @til
      _    _    _    _    _    _    home pgdn pgup end  _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _
      _    _    _              @ltab          _    _    _    _
    )
  '';
  mkTkl = mkKbd ''
    (defalias
      fst  (layer-toggle fst-layer)
      snd  (layer-toggle snd-layer)
      til  (around lsft grv)
      ltab (around lsft tab)
    )
    (defsrc
      esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12        ssrq slck pause
      grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc  ins  home pgup
      tab  q    w    e    r    t    y    u    i    o    p    [    ]    \     del  end  pgdn
      caps a    s    d    f    g    h    j    k    l    ;    '    ret
      lsft z    x    c    v    b    n    m    ,    .    /    rsft                 up
      lctl lmet lalt           spc            ralt rmet cmp  rctl            left down rght
    )
    (deflayer qwerty
      _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
      esc  _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _
      esc  _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _
      @fst _    _    _    _    _    _    _    _    _    _    _    _
      _    _    _    _    _    _    _    _    _    _    _    ret                  _
      _    _    _              _              _    _    _    _               _    _    _
    )
    (deflayer fst-layer
      _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
      _    f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  _     _    _    _
      @snd _    _    _    _    _    _    _    _    _    _    _    _    grv   _    _    _
      _    _    _    slck _    _    lft  down up   rght _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _                    _
      _    _    ralt           tab            _    _    _    _               _    _    _
    )
    (deflayer snd-layer
      _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
      _    brdn bru  _    _    _    _    prev pp   next mute vold volu _     _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _    _    @til  _    _    _
      _    _    _    _    _    _    home pgdn pgup end  _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _                    _
      _    _    _              @ltab          _    _    _    _               _    _    _
    )
  '';
  mk100 = mkKbd ''
    (defalias
      fst  (layer-toggle fst-layer)
      snd  (layer-toggle snd-layer)
      til  (around lsft grv)
      ltab (around lsft tab)
    )
    (defsrc
      esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12        ssrq slck pause
      grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc  ins  home pgup  nlck kp/  kp*  kp-
      tab  q    w    e    r    t    y    u    i    o    p    [    ]    \     del  end  pgdn  kp7  kp8  kp9  kp+
      caps a    s    d    f    g    h    j    k    l    ;    '    ret                        kp4  kp5  kp6
      lsft z    x    c    v    b    n    m    ,    .    /    rsft                 up         kp1  kp2  kp3  kprt
      lctl lmet lalt           spc            ralt rmet cmp  rctl            left down rght  kp0  kp.
    )
    (deflayer qwerty
      _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
      esc  _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _     _    _    _    _
      esc  _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _     _    _    _    _
      @fst _    _    _    _    _    _    _    _    _    _    _    _                          _    _    _
      _    _    _    _    _    _    _    _    _    _    _    ret                  _          _    _    _    _
      _    _    _              _              _    _    _    _               _    _    _     _    _
    )
    (deflayer fst-layer
      _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
      _    f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  _     _    _    _     _    _    _    _
      @snd _    _    _    _    _    _    _    _    _    _    _    _    grv   _    _    _     _    _    _    _
      _    _    _    slck _    _    lft  down up   rght _    _    _                          _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _                    _          _    _    _    _
      _    _    ralt           tab            _    _    _    _               _    _    _     _    _
    )
    (deflayer snd-layer
      _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
      _    brdn bru  _    _    _    _    prev pp   next mute vold volu _     _    _    _     _    _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _    _    @til  _    _    _     _    _    _    _
      _    _    _    _    _    _    home pgdn pgup end  _    _    _                          _    _    _
      _    _    _    _    _    _    _    _    _    _    _    _                    _          _    _    _    _
      _    _    _              @ltab          _    _    _    _               _    _    _     _    _
    )
  '';
in {
  imports =
    [
      (import "${nixos-hardware}/common/cpu/intel")
      (import "${nixos-hardware}/common/pc/laptop")
      (import "${nixos-hardware}/common/pc/laptop/ssd")
      (import "${home-manager}/nixos")
      (import "${kmonad-srv}/nix/nixos-module.nix")
      (import ./rigtora.nix)
    ]
    ++ (
      if builtins.pathExists ./../prv/nix/configuration.nix
      then [(import ./../prv/nix/configuration.nix)]
      else []
    );

  options.services.functora = with lib; {
    userName = mkOption {
      type = types.str;
    };
    blockHosts = mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = rec {
    #
    # Misc
    #
    environment.variables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
      BROWSER = "qutebrowser";
      TERMINAL = "alacritty";
      WLR_NO_HARDWARE_CURSORS = "1";
      NIXOS_OZONE_WL = "1";
    };
    environment.sessionVariables = environment.variables;
    #
    # Nix
    #
    nix.settings.cores = 1;
    nix.settings.max-jobs = 1;
    nix.settings.trusted-users = [
      "root"
      config.services.functora.userName
    ];
    nix.settings.trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    nix.settings.substituters = [
      "https://cache.iog.io"
    ];
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    security.polkit.enable = true;
    security.pam.services.swaylock = {};
    #
    # Nvidia
    #
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vulkan-validation-layers
        intel-media-driver # LIBVA_DRIVER_NAME=iHD
        vaapiIntel # LIBVA_DRIVER_NAME=i965
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
    #
    # Media
    #
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    hardware.keyboard.qmk.enable = true;

    #
    # TODO : remove when this is fixed
    # https://github.com/NixOS/nixpkgs/issues/180175
    #
    systemd.services.NetworkManager-wait-online = {
      serviceConfig = {
        ExecStart = ["" "${pkgs.networkmanager}/bin/nm-online -q"];
      };
    };

    services.kmonad = {
      enable = true;
      package = kmonad-pkg;
      keyboards.k995p = mk100 "/dev/input/by-id/usb-CATEX_TECH._104EC-XRGB_CA2017090001-event-kbd";
      keyboards.maxfit61 = mk60p "/dev/input/by-id/usb-FANTECH_MAXFIT61_Mechanical_Keyboard-event-kbd";
      keyboards.alice80 = {
        device = "/dev/input/by-id/usb-Telink_FEKER_Alice80-event-kbd";
        defcfg = {
          enable = true;
          fallthrough = true;
          allowCommands = false;
        };
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
      keyboards.md600 = {
        device = "/dev/input/by-id/usb-Mistel_MD600-event-kbd";
        defcfg = {
          enable = true;
          fallthrough = true;
          allowCommands = false;
        };
        config = ''
          (defalias
            fst  (layer-toggle fst-layer)
            snd  (layer-toggle snd-layer)
            til  (around lsft grv)
            ltab (around lsft tab)
          )
          (defsrc
            esc  1    2    3    4    5              6    7    8    9    0    -    =    bspc
            tab  q    w    e    r    t              y    u    i    o    p    [    ]    \
            caps a    s    d    f    g              h    j    k    l    ;    '    ret
            lsft z    x    c    v    b              n    m    ,    .    /    rsft
            lctl lmet lalt      spc                      spc       ralt      rctl
          )
          (deflayer qwerty
            _    _    _    _    _    _              _    _    _    _    _    _    _    _
            esc  _    _    _    _    _              _    _    _    _    _    _    _    _
            @fst _    _    _    _    _              _    _    _    _    _    _    _
            _    _    _    _    _    _              _    _    _    _    _    ret
            _    _    _         _                        _         _         _
          )
          (deflayer fst-layer
            _    f1   f2   f3   f4   f5             f6   f7   f8   f9   f10  f11  f12  _
            @snd _    _    _    _    _              _    _    _    _    _    _    _    grv
            _    _    _    slck _    _              lft  down up   rght _    _    _
            _    _    _    _    _    _              _    _    _    _    _    _
            _    _    ralt      tab                      tab       _         _
          )
          (deflayer snd-layer
            _    brdn bru  _    _    _              _    prev pp   next mute vold volu _
            _    _    _    _    _    _              _    _    _    _    _    _    _    @til
            _    _    _    _    _    _              home pgdn pgup end  _    _    _
            _    _    _    _    _    _              _    _    _    _    _    _
            _    _    _         @ltab                    @ltab     _         _
          )
        '';
      };
      keyboards.a275 = {
        device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
        defcfg = {
          enable = true;
          fallthrough = true;
          allowCommands = false;
        };
        config = ''
          (defalias
            fst  (layer-toggle fst-layer)
            snd  (layer-toggle snd-layer)
            til  (around lsft grv)
            ltab (around lsft tab)
          )
          (defsrc
            esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  home end  del
            grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
            caps a    s    d    f    g    h    j    k    l    ;    '    ret
            lsft z    x    c    v    b    n    m    ,    .    /    rsft
            wkup lctl lmet lalt           spc            ralt prnt rctl pgup up   pgdn
                                                                        left down rght
          )
          (deflayer qwerty
            _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _
            esc  _    _    _    _    _    _    _    _    _    _    _    _    _
            esc  _    _    _    _    _    _    _    _    _    _    _    _    _
            @fst _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _    _    _    _    _    _    _    _    _    ret
            lctl _    _    _              _              _    _    _    _    _    _
                                                                        _    _    _
          )
          (deflayer fst-layer
            _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _
            _    f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  _
            @snd _    _    _    _    _    _    _    _    _    _    _    _    grv
            _    _    _    slck _    _    lft  down up   rght _    _    _
            _    _    _    _    _    _    _    _    _    _    _    ret
            _    _    _    ralt           tab            _    _    _    _    _    _
                                                                        _    _    _
          )
          (deflayer snd-layer
            _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _    brdn bru  _    _    prev pp   next mute vold volu _
            _    _    _    _    _    _    _    _    _    _    _    _    _    @til
            _    _    _    _    _    _    home pgdn pgup end  _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _
            _    _    _    _              @ltab          _    _    _    _    _    _
                                                                        _    _    _
          )
        '';
      };
    };

    #
    # Android
    #
    programs.adb.enable = true;

    #
    # Web eID
    #
    # Tell p11-kit to load/proxy opensc-pkcs11.so, providing all available slots
    # (PIN1 for authentication/decryption, PIN2 for signing).
    environment.etc."pkcs11/modules/opensc-pkcs11".text = ''
      module: ${pkgs.opensc}/lib/opensc-pkcs11.so
    '';
    services.pcscd.enable = true;
    services.pcscd.plugins = [pkgs.acsccid];
    #
    # Firefox
    #
    programs.firefox = {
      enable = true;
      # nativeMessagingHosts.euwebid = true;
      nativeMessagingHosts.packages = [pkgs.web-eid-app];
      policies.SecurityDevices.p11-kit-proxy = "${pkgs.p11-kit}/lib/p11-kit-proxy.so";
    };
    #
    # Chromium
    #
    environment.etc."chromium/native-messaging-hosts/eu.webeid.json".source = "${pkgs.web-eid-app}/share/web-eid/eu.webeid.json";
    environment.etc."opt/chrome/native-messaging-hosts/eu.webeid.json".source = "${pkgs.web-eid-app}/share/web-eid/eu.webeid.json";
    environment.systemPackages = with pkgs; [
      # Wrapper script to tell to Chrome/Chromium to use p11-kit-proxy to load
      # security devices, so they can be used for TLS client auth.
      # Each user needs to run this themselves, it does not work on a system level
      # due to a bug in Chromium:
      #
      # https://bugs.chromium.org/p/chromium/issues/detail?id=16387
      (pkgs.writeShellScriptBin "setup-browser-eid" ''
        NSSDB="''${HOME}/.pki/nssdb"
        mkdir -p ''${NSSDB}

        ${pkgs.nssTools}/bin/modutil -force -dbdir sql:$NSSDB -add p11-kit-proxy \
          -libfile ${pkgs.p11-kit}/lib/p11-kit-proxy.so
      '')
      libdigidocpp
      qdigidoc
    ];

    services.tor.enable = true;
    services.tor.client.enable = true;
    networking.firewall.enable = true;
    networking.nameservers = ["8.8.8.8" "8.8.4.4"];
    virtualisation.virtualbox.host.enable = true;
    virtualisation.docker.enable = false;
    virtualisation.podman.enable = true;
    virtualisation.podman.dockerSocket.enable = true;
    virtualisation.podman.defaultNetwork.settings.dns_enabled = true;
    users.extraGroups.vboxusers.members = [config.services.functora.userName];
    networking.extraHosts = blocked-hosts;

    users.groups.plugdev = {};
    users.users.${config.services.functora.userName} = {
      isNormalUser = true;
      description = config.services.functora.userName;
      extraGroups = [
        "wheel"
        "input"
        "uinput"
        "docker"
        "podman"
        "plugdev"
        "adbusers"
        "networkmanager"
      ];
      packages = with pkgs; [
      ];
    };

    # xdg-desktop-portal works by exposing a series of D-Bus interfaces
    # known as portals under a well-known name
    # (org.freedesktop.portal.Desktop) and object path
    # (/org/freedesktop/portal/desktop).
    # The portal interfaces include APIs for file access, opening URIs,
    # printing and others.
    services.dbus.enable = true;
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      # gtk portal needed to make gtk apps happy
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
      config.common.default = "*";
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
        bemenu
        wlr-randr
        swaylock
        swayidle
        wl-clipboard
        mako
        wofi
        waybar
        dbus-sway-environment
        configure-gtk
        xdg-utils
        glib
        dracula-theme
        gnome3.adwaita-icon-theme
        #
        # apps
        #
        exfat
        jmtpfs
        shellcheck
        chromium
        xournalpp
        gnome.nautilus
        ccrypt
        awscli2
        libreoffice
        tor-browser-bundle-bin
        kooha
        mpv
        qmk
        qmk-setup
        prusa-slicer
        cura
        freecad
        lesspass-cli
        # mkdir -p ~/macos/Public
        # cd ~/macos
        # chmod 777 ./Public
        # quickget macos monterey
        # quickemu --vm macos-monterey.conf --public-dir ./Public --extra_args "-cpu host,+vmx"
        quickemu
        pavucontrol
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
        # ".xkb" = {
        #   source = ./xkb;
        #   recursive = true;
        # };
        ".config/nvim/coc-settings.json".source = ../cfg/coc-settings.json;
      };
      programs.i3status-rust = {
        enable = true;
        bars.bottom.blocks = [
          {
            block = "battery";
            format = " BAT $percentage $time ";
            full_format = " BAT FULL ";
            icons_format = "";
          }
          {
            block = "backlight";
            format = " BRT $brightness ";
            icons_format = "";
          }
          {
            block = "net";
            format = " $ssid ";
            icons_format = "";
          }
          {
            block = "disk_space";
            info_type = "available";
            alert_unit = "GB";
            alert = 10.0;
            warning = 20.0;
            format = " SSD $available ";
            icons_format = "";
          }
          {
            block = "memory";
            format = " RAM $mem_used_percents.eng(w:1) ";
            icons_format = "";
          }
          {
            block = "cpu";
            format = " CPU $utilization ";
            icons_format = "";
          }
          {
            block = "sound";
            format = " VOL $volume ";
            show_volume_when_muted = true;
            icons_format = "";
          }
          {
            block = "time";
            format = " $timestamp.datetime(f:'%a %Y-%m-%d %R') ";
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
            {
              criteria = "eDP-1";
              status = "disable";
            }
            {criteria = "HDMI-A-1";}
          ];
          undocked.outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
            }
          ];
        };
      };
      wayland.windowManager.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraOptions = ["--unsupported-gpu"];
        extraConfig = ''
          for_window [class="Alacritty"] fullscreen enable
          for_window [class="qutebrowser"] fullscreen enable
          for_window [class="mpv"] fullscreen enable
          assign [class="qutebrowser"] workspace 10
          assign [class="mpv"] workspace 8

          bindsym Mod4+Shift+z mode "hotkeygrab"
          mode "hotkeygrab" {
            bindsym Mod4+Shift+z mode "default"
          }

          exec ${dbus-sway-environment}/bin/dbus-sway-environment
          exec ${configure-gtk}/bin/configure-gtk
        '';
        #
        # TODO : need to improve dbus-sway-environment and configure-gtk
        # because seems like they are giving desired result only
        # after manual config reload inside the sway session.
        #
        config = with pkgs; let
          mod = "Mod4";
          wmEx = x: "exec --no-startup-id ${x}";
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
            "${x}XF86MonBrightnessDown" = newBrightness "3-";
            "${x}XF86MonBrightnessUp" = newBrightness "+3";
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
            "${mod}+Shift+u" = "exec ${pkgs.warpd}/bin/warpd --hint";
            "${mod}+Shift+i" = "exec ${pkgs.warpd}/bin/warpd --normal";
            "${mod}+Shift+y" = "exec ${pkgs.warpd}/bin/warpd --grid";
            "${mod}+0" = "workspace number 10";
            "${mod}+Shift+0" = "move container to workspace number 10";
          };
        in {
          modifier = mod;
          menu = "bemenu-run";
          defaultWorkspace = "workspace number 1";
          #
          # TODO : remove when kanshi service bug is fixed:
          # https://github.com/nix-community/home-manager/issues/2797
          #
          startup = [
            {
              command = "${
                pkgs.systemd
              }/bin/systemctl --user reload-or-restart kanshi.service";
              always = true;
            }
          ];
          keybindings = lib.mkOptionDefault (
            newMediaKeys "Mod5+Shift+"
            // newMediaKeys ""
            // cfgProgrKeys
            // cfgBasicKeys
          );
          bars = [
            {
              mode = "dock";
              position = "bottom";
              trayOutput = "none";
              statusCommand = "${i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-bottom.toml";
            }
          ];
          seat = {"*" = {hide_cursor = "2000";};};
          input = {
            "*" = {
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
            };
          };
        };
      };
    };
    #
    # Automount
    #
    services.gvfs.enable = true;
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
      displayManager.sessionPackages = [pkgs.sway];
    };
  };
}
