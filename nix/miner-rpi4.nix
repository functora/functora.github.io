{
  config,
  pkgs,
  lib,
  ...
}: let
  osUsr = "TODO";
  osPwd = "TODO";
  wifiNet = "TODO";
  wifiPwd = "TODO";
  xmrAddr = "TODO";
  xmrSolo = false;
  machine = "miner-rpi4";
  archive = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixos-hardware/archive/f38f9a4c9b2b6f89a5778465e0afd166a8300680.tar.gz";
    sha256 = "079i605dlcgvw5kd2f8jnqq9ms81qf9ln0z6n8vcyyxn03zj0aal";
  };
in {
  imports = ["${archive}/raspberry-pi/4"];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  networking = {
    hostName = machine;
    wireless = {
      enable = true;
      networks."${wifiNet}".psk = wifiPwd;
      interfaces = ["wlan0"];
    };
  };

  users = {
    mutableUsers = false;
    users."${osUsr}" = {
      isNormalUser = true;
      password = osPwd;
      extraGroups = ["wheel"];
    };
  };

  boot.cleanTmpDir = true;
  nix.settings.auto-optimise-store = true;
  environment.systemPackages = with pkgs; [vim btop];
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
    pools = [
      {
        url = "pool.hashvault.pro:80";
        coin = "XMR";
        user =
          if xmrSolo
          then "solo:${xmrAddr}"
          else xmrAddr;
        nicehash = false;
        keepalive = false;
        tls = true;
        tls-fingerprint = "420c7850e09b7c0bdcf748a7da9eb3647daf8515718f36d9ccfdd6b9ff834b14";
      }
    ];
  };
}
