{
  config,
  pkgs,
  lib,
  ...
}: let
  # machine = "TODO";
  # wifiNet = "TODO";
  # wifiPwd = "TODO";
  xmrAddr = config.services.rigtora.xmrAddr;
  xmrSolo = config.services.rigtora.xmrSolo;
in {
  imports = [
  ];

  options.services.rigtora = with lib; {
    enable = mkEnableOption "rigtora";
    xmrMine = mkOption {
      type = types.bool;
      default = true;
    };
    xmrAddr = mkOption {
      type = types.str;
      default = "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG";
    };
    xmrSolo = mkOption {
      type = types.bool;
      default = false;
    };
    cpuName = mkOption {
      type = types.str;
    };
    openSsh = mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = lib.mkIf config.services.rigtora.enable {
    # networking = {
    #   hostName = machine;
    #   wireless = {
    #     enable = true;
    #     networks."${wifiNet}".psk = wifiPwd;
    #     interfaces = [ "wlan0" ];
    #   };
    # };
    networking.firewall.enable = true;
    boot.tmp.cleanOnBoot = true;
    nix.settings.auto-optimise-store = true;
    #
    # TODO : script to derive cpuName
    #
    environment.systemPackages = with pkgs; [
      git
      vim
      htop
      cpuid
      udiskie
      litecli
    ];
    services.journald.extraConfig = ''
      SystemMaxUse=100M
      MaxFileSec=7day
    '';

    services.fail2ban.enable = true;
    services.openssh = {
      enable = config.services.rigtora.openSsh;
      settings.PasswordAuthentication = false;
      settings.KbdInteractiveAuthentication = false;
      settings.PermitRootLogin = "no";
    };
    #
    # Automount
    #
    services.gvfs.enable = true;
    services.udisks2.enable = true;
    #
    # XMR
    #
    services.xmrig.enable = config.services.rigtora.xmrMine;
    services.xmrig.settings = {
      cpu.enabled = true;
      autosave = false;
      opencl = false;
      cuda = false;
      pools = [
        {
          url = "pool.hashvault.pro:80";
          coin = "XMR";
          user =
            if xmrSolo
            then "solo:${xmrAddr}"
            else xmrAddr;
          pass = config.services.rigtora.cpuName;
          nicehash = false;
          keepalive = false;
          tls = true;
          tls-fingerprint = "420c7850e09b7c0bdcf748a7da9eb3647daf8515718f36d9ccfdd6b9ff834b14";
        }
      ];
    };
  };
}
