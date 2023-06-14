{ config, pkgs, lib, ... }:

let
  # machine = "TODO";
  # wifiNet = "TODO";
  # wifiPwd = "TODO";
  xmrAddr = config.services.rigtora.xmrAddr;
  xmrSolo = config.services.rigtora.xmrSolo;
in {
  imports = [

  ];

  options.services.rigtora = with lib; {
    xmrAddr = mkOption {
      type = types.str;
    };
    xmrSolo = mkOption {
      type = types.bool;
    };
  };

  # networking = {
  #   hostName = machine;
  #   wireless = {
  #     enable = true;
  #     networks."${wifiNet}".psk = wifiPwd;
  #     interfaces = [ "wlan0" ];
  #   };
  # };

  boot.cleanTmpDir = true;
  nix.settings.auto-optimise-store = true;
  environment.systemPackages = with pkgs; [ vim htop ];
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

  services.fail2ban.enable = true;
  services.openssh.enable = true;
  services.xmrig.enable = true;
  services.xmrig.settings = {
    autosave = true;
    opencl = false;
    cuda = false;
    cpu = true;
    pools = [{
      url = "pool.hashvault.pro:80";
      coin = "XMR";
      user = if xmrSolo then "solo:${xmrAddr}" else xmrAddr;
      nicehash = false;
      keepalive = false;
      tls = true;
      tls-fingerprint = "420c7850e09b7c0bdcf748a7da9eb3647daf8515718f36d9ccfdd6b9ff834b14";
    }];
  };
}
