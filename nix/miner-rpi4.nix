{ config, pkgs, lib, ... }:

let
  osUsr = "TODO";
  osPwd = "TODO";
  wifiNet = "TODO";
  wifiPwd = "TODO";
  xmrAddr = "TODO";
  machine = "miner-rpi4";
  archive = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixos-hardware/archive/f38f9a4c9b2b6f89a5778465e0afd166a8300680.tar.gz";
    sha256 = "079i605dlcgvw5kd2f8jnqq9ms81qf9ln0z6n8vcyyxn03zj0aal";
  };
in {
  imports = [ "${archive}/raspberry-pi/4" ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  networking = {
    hostName = machine;
    wireless = {
      enable = true;
      networks."${wifiNet}".psk = wifiPwd;
      interfaces = [ "wlan0" ];
    };
  };

  users = {
    mutableUsers = false;
    users."${osUsr}" = {
      isNormalUser = true;
      password = osPwd;
      extraGroups = [ "wheel" ];
    };
  };

  environment.systemPackages = with pkgs; [ vim htop ];
  services.fail2ban.enable = true;
  services.openssh.enable = true;
  services.xmrig.enable = true;
  services.xmrig.settings = {
    autosave = true;
    opencl = false;
    cuda = false;
    cpu = true;
    pools = [{
      url = "xmr-us-east1.nanopool.org:14444";
      coin = "XMR";
      user = xmrAddr;
    }];
  };
}
