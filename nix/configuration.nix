{
  lib,
  pkgs,
  config,
  ...
}: let
  vi = import ./../pub/vi/nix/default.nix {};
  fj = import ./firejail.nix;
  dns = ["8.8.8.8" "8.8.4.4"];
  unst = import ./nixpkgs-unstable.nix;
  vkdoom = import ./vkdoom.nix {inherit pkgs;};
  # xkb = pkgs.writeText "xkb-layout" (builtins.readFile ./../cfg/.Xmodmap);
  # yewtube = import ./yewtube.nix;
  qmk-setup = import ./qmk-setup.nix;
  lockCmd = "${pkgs.swaylock}/bin/swaylock --color=000000";
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/8d5e27b4807d25308dfe369d5a923d87e7dbfda3.tar.gz";
    sha256 = "05b1g64ra54yrpy8nrwsccrbrbns6v557lqwjnm9xwjlcn7nkc39";
  };
  nixos-hardware = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixos-hardware/archive/fa194fc484fd7270ab324bb985593f71102e84d1.tar.gz";
    sha256 = "06yn179lbhql3vkk4cjca4mdwr6lfdh6n1vqma3a4266dap6hcf4";
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
  blocked-hosts =
    builtins.concatStringsSep "\n"
    (builtins.map (x: "127.0.0.1 ${x} www.${x} www2.${x} web.${x} rus.${x} news.${x}")
      (
        if config.services.functora.blockHosts
        then [
          "err.ee"
          "delfi.ee"
          "postimees.ee"
          # "rumble.com"
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
  mkOpenArena = mod:
    fj.mkFirejailCustom {
      pkg = "openarena-${mod}";
      dir = "q3";
      exe = ''
        ${pkgs.openarena}/bin/openarena \
          +set fs_homepath ~/.firejail/q3/.openarena \
          +set fs_game ${mod}
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
      _    _    _    _    _    _    _    sdn  sup  _    _    _                    _
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
  mkAlice = mkKbd ''
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
      lctl      lmet      spc       lalt      spc       ralt      rctl           lft  down rght
    )
    (deflayer qwerty
      _    _    _    _    _    _    _              _    _    _    _    _    _    _
      esc  _    _    _    _    _              _    _    _    _    _    _    _    _         _
      @fst _    _    _    _    _              _    _    _    _    _    _    _              _
      _    _    _    _    _    _              _    _    _    _    _    _    ret       _    _
      _         _         _         _         _         _         _              _    _    _
    )
    (deflayer fst-layer
      _    f1   f2   f3   f4   f5   f6             f7   f8   f9   f10  f11  f12  _
      @snd _    _    _    _    _              _    _    _    _    _    _    _    grv       _
      _    _    _    slck _    _              lft  down up   rght _    _    _              _
      _    _    _    _    _    _              _    _    _    _    _    _    _         _    _
      _         _         tab       ralt      tab       _         _              _    _    _
    )
    (deflayer snd-layer
      _    brdn bru  _    _    _    _              prev pp   next mute vold volu _
      _    _    _    _    _    _              _    _    _    _    _    _    _    @til      _
      _    _    _    _    _    _              home pgdn pgup end  _    _    _              _
      _    _    _    _    _    _              _    _    _    _    _    _    _         _    _
      _         _         @ltab     _         @ltab     _         _              _    _    _
    )
  '';
  baseBorg = {
    repo = "/mnt/backup/borg";
    user = config.services.functora.userName;
    doInit = true;
    startAt = "daily";
    compression = "auto,lzma";
    encryption.mode = "none";
    prune.keep = {
      daily = 7;
      weekly = 4;
      monthly = 3;
    };
  };
  mkLocalBorg = cfg: {
    "local-${cfg.name}" =
      baseBorg
      // {
        paths = cfg.paths;
      };
  };
in {
  imports =
    [
      # (import "${nixos-hardware}/common/cpu/intel")
      # (import "${nixos-hardware}/common/pc/laptop")
      # (import "${nixos-hardware}/common/pc/laptop/ssd")
      (import "${home-manager}/nixos")
      (import ./rigtora.nix)
    ]
    ++ (import ./tabby-services.nix)
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
    localBorg = mkOption {
      type = types.listOf (types.submodule {
        options.name = mkOption {
          type = types.str;
        };
        options.paths = mkOption {
          type = types.listOf types.str;
        };
      });
      default = [];
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
      ROC_ENABLE_PRE_VEGA = "1";
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
    # GPU
    #
    systemd.tmpfiles.rules = let
      rocmEnv = pkgs.symlinkJoin {
        name = "rocm-combined";
        paths = with pkgs.rocmPackages; [
          rocblas
          hipblas
          clr
        ];
      };
    in [
      "L+    /opt/rocm   -    -    -     -    ${rocmEnv}"
    ];
    hardware.opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        amdvlk
        rocmPackages.clr.icd
        rocmPackages.clr
        rocmPackages.rocminfo
        rocmPackages.rocm-runtime
      ];
      extraPackages32 = with pkgs; [
        driversi686Linux.amdvlk
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

    #
    # Productivity
    #
    services.unbound.enable = config.services.functora.blockHosts;
    services.unbound.settings.server.interface = ["127.0.0.1"];
    services.unbound.settings.server.port = 5335;
    services.adguardhome.enable = config.services.functora.blockHosts;
    services.adguardhome.mutableSettings = false;
    services.adguardhome.settings.dns.upstream_dns = ["127.0.0.1:5335"];
    services.adguardhome.settings.dns.bootstrap_dns = dns;
    services.adguardhome.settings.filtering = {
      filtering_enabled = true;
      blocked_services.ids = [
        "4chan"
        "500px"
        "9gag"
        "activision_blizzard"
        "aliexpress"
        "amazon_streaming"
        # "amazon"
        "amino"
        "apple_streaming"
        "battle_net"
        "betano"
        "betfair"
        "betway"
        "bigo_live"
        "bilibili"
        "blaze"
        "blizzard_entertainment"
        "bluesky"
        "box"
        "claro"
        # "cloudflare"
        "clubhouse"
        "coolapk"
        "crunchyroll"
        "dailymotion"
        "deezer"
        "directvgo"
        "discord"
        "discoveryplus"
        "disneyplus"
        "douban"
        "dropbox"
        "ebay"
        "electronic_arts"
        "epic_games"
        "espn"
        "facebook"
        "fifa"
        "flickr"
        "globoplay"
        "gog"
        "hbomax"
        "hulu"
        "icloud_private_relay"
        "iheartradio"
        "imgur"
        "instagram"
        "iqiyi"
        "kakaotalk"
        "kik"
        "kook"
        "lazada"
        "leagueoflegends"
        "line"
        "linkedin"
        "lionsgateplus"
        "looke"
        "mail_ru"
        "mastodon"
        "mercado_libre"
        "minecraft"
        "nebula"
        "netflix"
        "nintendo"
        "nvidia"
        "ok"
        "olvid"
        "onlyfans"
        "origin"
        "paramountplus"
        "pinterest"
        "playstation"
        "plenty_of_fish"
        "plex"
        "pluto_tv"
        "privacy"
        "qq"
        "rakuten_viki"
        "reddit"
        "riot_games"
        "roblox"
        "rockstar_games"
        "samsung_tv_plus"
        "shein"
        "shopee"
        "signal"
        "skype"
        "snapchat"
        # "soundcloud"
        "spotify"
        "steam"
        # "telegram"
        "temu"
        "tidal"
        "tiktok"
        "tinder"
        "tumblr"
        "twitch"
        "twitter"
        "ubisoft"
        "valorant"
        "viber"
        "vimeo"
        "vk"
        "voot"
        "wargaming"
        "wechat"
        "weibo"
        "whatsapp"
        "wizz"
        "xboxlive"
        "xiaohongshu"
        "youtube"
        "yy"
        "zhihu"
      ];
      blocked_services.schedule = let
        unBlock = {
          start = "19h";
          end = "24h";
        };
      in {
        mon = unBlock;
        tue = unBlock;
        wed = unBlock;
        thu = unBlock;
        fri = unBlock;
        sat = unBlock;
        sun = unBlock;
        time_zone = "Local";
      };
    };

    #
    # Keyboards
    #
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
      keyboards.miniM = mkTkl "/dev/input/by-id/usb-Unicomp_Inc_U_AP1_4_87k_Kbrd_v7_57-event-kbd";
      keyboards.k995p = mk100 "/dev/input/by-id/usb-CATEX_TECH._104EC-XRGB_CA2017090001-event-kbd";
      keyboards.maxfit61 = mk60p "/dev/input/by-id/usb-FANTECH_MAXFIT61_Mechanical_Keyboard-event-kbd";
      keyboards.feker80 = mkAlice "/dev/input/by-id/usb-Telink_FEKER_Alice80-event-kbd";
      keyboards.aks068 = mkAlice "/dev/input/by-id/usb-RDR_AKS068-event-kbd";
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
    # Services
    #
    programs.adb.enable = true;
    programs.ydotool.enable = true;
    services.tor.enable = true;
    hardware.sane.enable = true;
    hardware.sane.extraBackends = [pkgs.hplip];
    # hardware.sane.extraBackends = [pkgs.sane-airscan];
    services.ipp-usb.enable = true;
    services.printing.enable = true;
    services.printing.drivers = [pkgs.hplip];
    services.fail2ban.enable = true;
    services.tor.client.enable = true;
    networking.firewall.enable = true;
    networking.nameservers = dns;
    virtualisation.virtualbox.host.enable = true;
    virtualisation.containers.enable = true;
    virtualisation.podman.enable = true;
    virtualisation.podman.dockerCompat = true;
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
        "podman"
        "plugdev"
        "adbusers"
        "networkmanager"
        "scanner"
        "ydotool"
        "render"
        "video"
        "lp"
      ];
      #
      # Web eID
      #
      packages = with pkgs; [
        qdigidoc # Digidoc
        web-eid-app # Signing in browswer
        p11-kit # Signing in browswer
        opensc # Signing in browswer
      ];
    };
    services.pcscd.enable = true;
    programs.firefox.enable = true;
    programs.firefox.package = pkgs.firefox-esr;
    programs.firefox.nativeMessagingHosts.packages = [pkgs.web-eid-app];
    programs.firefox.policies.SecurityDevices.p11-kit-proxy = "${pkgs.p11-kit}/lib/p11-kit-proxy.so";
    environment.etc."pkcs11/modules/opensc-pkcs11".text = ''
      module: ${pkgs.opensc}/lib/opensc-pkcs11.so
    '';

    # xdg-desktop-portal works by exposing a series of D-Bus interfaces
    # known as portals under a well-known name
    # (org.freedesktop.portal.Desktop) and object path
    # (/org/freedesktop/portal/desktop).
    # The portal interfaces include APIs for file access, opening URIs,
    # printing and others.
    # If still there are some issues, try to rm -rf ~/.config/dconf
    services.dbus.enable = true;
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      # gtk portal needed to make gtk apps happy
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
      config.common.default = "*";
    };

    #
    # Via/Vial
    #
    services.udev.packages = with pkgs; [
      via
    ];

    #
    # Firejail
    #
    programs.firejail.enable = true;
    programs.firejail.wrappedBinaries =
      fj.mkFirejailSimple "chromium"
      // mkOpenArena "rat"
      // import ./doom.nix
      // fj.mkFirejailCustom {
        pkg = "doom-infinite";
        dir = "doom";
        exe = ''
          ${pkgs.gzdoom}/bin/gzdoom \
            -iwad ./freedoom-0.13.0/freedoom2.wad \
            -file ./DOOM_Infinite_DEMO_0978_6.pk3 \
            -file "./duhd/1 lights2.wad" "./duhd/8 DHTP Textures.pk3" "./duhd/9 JFO.wad" "./duhd/10 HD_SFX.wad" "./duhd/12 Flashlight++.pk3" "./duhd/13 Tilt++.pk3" "./duhd/14 brightmaps2.wad" "./duhd/16 d3snds.wad" "./duhd/17 brutaldoom_stuff.wad" "./duhd/19 SpriteShadow.wad" "./duhd/20 WorldGamma.wad" "./duhd/21 BloomBoost.wad" "./duhd/22 MotionBlur.pk3" "./duhd/23 hires_decals.wad" "./duhd/24 Terrains.wad" "./duhd/25 HD HUD.pk3" "./duhd/26 Liquids.pk3" "./duhd/27 marcelus_hd_sprites.pk3" "./duhd/29 Universal Rain and Snow v3.pk3" "./duhd/30 OST Remake.pk3" "./duhd/31 texture_lights.wad" "./duhd/0 Parallax PBR.pk3" \
            -file ./DoomBSMS.wad \
            -file ./mod.pk3 \
            -file ./UniversalGibs-master.zip \
            -file ./CodeFX.pk3 \
            -file ./CodeFXFire.pk3 \
            -file ./CodeFXBlood.pk3 \
            -file "./liquid/Liquid Texture Pack/(GZDoom) Liquid Texture Pack V4.0.pk3" \
            -file "./liquid/Liquid Texture Pack/LTP V4.0 Glowing Toxic Texture Addon.pk3" \
            -file "./liquid/LTP V4.0 Shader pack.pk3" \
            -file "./liquid/LTP V4.0 Sky shader addon.pk3" \
            -file ./SimpleSlots.1.1.pk7
        '';
      }
      // fj.mkFirejailCustom {
        pkg = "doom-mall";
        dir = "doom";
        exe = ''
          ${unst.gzdoom}/bin/gzdoom \
            -iwad ./freedoom-0.13.0/freedoom2.wad \
            -file ./brutalv22test4.pk3 \
            -file ./mall.pk3 \
            -file ./SimpleSlots.1.1.pk7
        '';
      }
      // fj.mkFirejailCustom {
        pkg = "tabby-download-embed";
        dir = "tabby-download";
        net = true;
        exe = ''
          ${import ./tabby.nix}/bin/tabby \
            download --model Nomic-Embed-Text
        '';
      }
      // fj.mkFirejailCustom {
        pkg = "tabby-download-deepseek";
        dir = "tabby-download";
        net = true;
        #
        # NOTE : DeepSeekCoder-6.7B is out of memory on 4GB GPU.
        # Seems like only models up to 3B size are supported.
        #
        exe = ''
          ${import ./tabby.nix}/bin/tabby \
            download --model DeepSeekCoder-1.3B
        '';
      }
      // fj.mkFirejailCustom {
        pkg = "tabby-download-gemma";
        dir = "tabby-download";
        net = true;
        exe = ''
          ${import ./tabby.nix}/bin/tabby \
            download --model CodeGemma-2B
        '';
      }
      // fj.mkFirejailCustom {
        pkg = "tabby-download-instruct";
        dir = "tabby-download";
        net = true;
        exe = ''
          ${import ./tabby.nix}/bin/tabby \
            download --model Qwen2.5-Coder-1.5B-Instruct
        '';
      }
      // fj.mkFirejailCustom {
        pkg = "tabby-agent";
        dir = "tabby";
        exe = "${
          import ./tabby-agent.nix {
            sock = "/home/${
              config.services.functora.userName
            }/.firejail/tabby/tabby.sock";
          }
        }/bin/tabby-agent";
      }
      // fj.mkFirejailCustom {
        pkg = "tabby-admin";
        dir = "tabby";
        exe = "${
          import ./tabby-admin.nix {sock = "./tabby.sock";}
        }/bin/tabby-admin";
      }
      // fj.mkFirejailOffline {
        pkg = "piper";
        exe = "${import ./piper.nix}/bin/piper";
      }
      // fj.mkFirejailOffline {
        pkg = "vi";
        exe = "${
          import ./vi-socat.nix {
            sock = "/home/${
              config.services.functora.userName
            }/.firejail/tabby/tabby.sock";
          }
        }/bin/vi";
      }
      // fj.mkFirejailOffline {
        pkg = "hoogle-w3m";
        exe = "${import ./hoogle-w3m.nix}/bin/hoogle-w3m";
      };
    #
    # Home
    #
    home-manager.users.${config.services.functora.userName} = {
      home.stateVersion = "22.11";
      home.packages = with pkgs; [
        tree
        s-tui
        qutebrowser
        xorg.xev
        pkgs.yewtube
        niv
        zip
        unzip
        p7zip
        unrar-free
        pciutils
        docker-client
        docker-compose
        btop-rocm
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
        adwaita-icon-theme
        #
        # apps
        #
        exfat
        jmtpfs
        shellcheck
        xournalpp
        nautilus
        ccrypt
        libreoffice
        tor-browser-bundle-bin
        kooha
        mpv
        qmk
        qmk-setup
        # cura
        git-lfs
        lesspass-cli
        # mkdir -p ~/macos/Public
        # cd ~/macos
        # chmod 777 ./Public
        # quickget macos monterey
        # quickemu --vm macos-monterey.conf --public-dir ./Public --extra_args "-cpu host,+vmx"
        quickemu
        pavucontrol
        via
        vial
        usbutils
        simple-scan
        system-config-printer
        pulsemixer
        (import ./vidmaker.nix)
        (import ./clipmaker.nix)
        neovim
      ];
      programs.git = {
        enable = true;
        lfs.enable = true;
        userName = "functora";
        userEmail = "functora@proton.me";
      };
      programs.alacritty = {
        enable = true;
        settings = {
          font.size = 14;
          window.startup_mode = "Fullscreen";
          terminal.shell.program = "${pkgs.bash}/bin/bash";
          terminal.shell.args = [
            "-c"
            "${pkgs.tmux}/bin/tmux attach || ${pkgs.tmux}/bin/tmux"
          ];
        };
      };
      programs.tmux = {
        enable = true;
        keyMode = "vi";
        baseIndex = 1;
        clock24 = true;
        extraConfig = ''
          set -s escape-time 0
        '';
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
        extraConfig = let
          ydotool = "YDOTOOL_SOCKET=/run/ydotoold/socket ${
            pkgs.ydotool
          }/bin/ydotool";
        in ''
          for_window [class="Alacritty"] fullscreen enable
          for_window [class="qutebrowser"] fullscreen enable
          for_window [class="mpv"] fullscreen enable
          assign [class="qutebrowser"] workspace 10
          assign [class="mpv"] workspace 8


          bindsym Mod4+Shift+i mode default, exec '${
            pkgs.wl-kbptr
          }/bin/wl-kbptr -o general.home_row_keys=qnhfjklmdas'
          bindsym Mod4+Shift+u mode mouse
          mode mouse {

              bindsym h exec '${ydotool} mousemove -x -35 -y   0'
              bindsym j exec '${ydotool} mousemove -x   0 -y  35'
              bindsym k exec '${ydotool} mousemove -x   0 -y -35'
              bindsym l exec '${ydotool} mousemove -x  35 -y   0'

              bindsym n exec '${ydotool} mousemove -w -- -1  0'
              bindsym m exec '${ydotool} mousemove -w --  0 -1'
              bindcode 59 exec '${ydotool} mousemove -w -- 0 1'
              bindcode 60 exec '${ydotool} mousemove -w -- 1 0'

              bindsym d exec '${ydotool} click 0x40'
              bindsym --release d exec '${ydotool} click 0x80'
              bindsym a exec '${ydotool} click 0x41'
              bindsym --release a exec '${ydotool} click 0x81'
              bindsym s exec '${ydotool} click 0x42'
              bindsym --release s exec '${ydotool} click 0x82'

              bindsym Escape mode default
          }

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
            "${mod}+y" = "exec ${alacritty}/bin/alacritty -e ${pkgs.yewtube}/bin/yt";
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
            "${mod}+0" = "workspace number 10";
            "${mod}+Shift+0" = "move container to workspace number 10";
          };
        in {
          modifier = mod;
          menu = "bemenu-run";
          defaultWorkspace = "workspace number 1";
          output = {
            DVI-D-1 = {
              mode = "1280x720@60.000Hz";
              # mode = "1920x1080@60.000Hz";
            };
          };
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
      videoDrivers = ["amdgpu"];
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = true;
      displayManager.defaultSession = "sway";
      displayManager.sessionPackages = [pkgs.sway];
    };
    #
    # Borg
    #
    services.borgbackup.jobs =
      lib.mkMerge
      (map mkLocalBorg config.services.functora.localBorg);
    #
    # AI
    #
    services.ollama.enable = true;
    services.ollama.acceleration = "rocm";
    # services.tabby-server.enable = true;
    # services.tabby-socket.enable = true;
  };
}
