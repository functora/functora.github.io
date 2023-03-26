{ config, pkgs, lib, ... }:

let
  osUsr = "TODO";
  osPwd = "TODO";
  wifiNet = "TODO";
  wifiPwd = "TODO";
  xmrAddr = "TODO";
  machine = "miner-rpi4";
  archive = builtins.fetchTarball {
    src = "https://github.com/NixOS/nixos-hardware/archive/f38f9a4c9b2b6f89a5778465e0afd166a8300680.tar.gz";
    sha256 = "0247y549c6v34lhv3rr5dxissixpzzzp3jmc6gsviy6pdr27w2yf";
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

  hardware.pulseaudio.enable = true;
  hardware.raspberry-pi."4".fkms-3d.enable = true;
  environment.systemPackages = with pkgs; [ vim ];

  services.xmrig.enable = true;
  services.xmrig.settings = {
    autosave = true;
    cpu = true;
    opencl = false;
    cuda = false;
    pools = [{
      url = "pool.supportxmr.com:443";
      user = xmrAddr;
      keepalive = true;
      tls = true;
    }];
  };
}
