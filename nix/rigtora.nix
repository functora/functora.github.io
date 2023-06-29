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
  cpuLoad = config.services.rigtora.cpuLoad;
in {
  imports = [
  ];

  options.services.rigtora = with lib; {
    enable = mkEnableOption "rigtora";
    xmrAddr = mkOption {
      type = types.str;
    };
    xmrSolo = mkOption {
      type = types.bool;
      default = false;
    };
    cpuName = mkOption {
      type = types.str;
    };
    cpuLoad = mkOption {
      type = types.int;
      default = null;
    };
    openSsh = mkOption {
      type = types.bool;
      default = false;
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
    boot.tmp.cleanOnBoot = true;
    nix.settings.auto-optimise-store = true;
    #
    # TODO : script to derive cpuName
    #
    environment.systemPackages = with pkgs; [vim htop cpuid];
    services.journald.extraConfig = ''
      SystemMaxUse=100M
      MaxFileSec=7day
    '';

    services.fail2ban.enable = true;
    services.openssh.enable = config.services.rigtora.openSsh;
    services.xmrig.enable = true;
    services.xmrig.settings = {
      autosave = false;
      opencl = false;
      cuda = false;
      cpu =
        {
          enabled = true;
        }
        // (
          if cpuLoad == null
          then {}
          else {priority = cpuLoad;}
        );
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
