#
# NOTE : Install with the command:
#
# curl -L https://raw.githubusercontent.com/functora/functora.github.io/refs/heads/master/nix/rpi4.nix > /etc/nixos/configuration.nix
#
# NOTE : There is HDMI/wifi interference issue on rpi4. If wifi is not working, set lower resolution:
#
# - Mount boot partition.
# - Add hdmi_safe=1 line to config.txt.
# - Reboot.
#
{
  config,
  pkgs,
  lib,
  ...
}: let
  user = "TODO";
  password = "TODO";
  SSID = "TODO";
  SSIDpassword = "TODO";
  hostname = "TODO";
in {
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = ["xhci_pci" "usbhid" "usb_storage"];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  networking = {
    hostName = hostname;
    wireless = {
      enable = true;
      networks."${SSID}".psk = SSIDpassword;
      interfaces = ["wlan0"];
    };
  };

  users = {
    mutableUsers = false;
    users."${user}" = {
      isNormalUser = true;
      password = password;
      extraGroups = ["wheel"];
    };
  };

  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "23.11";

  #
  # Apps
  #
  environment.systemPackages = with pkgs; [
    git
    vim
    htop
    cpuid
    udiskie
    litecli
    libraspberrypi
  ];
  #
  # Storage
  #
  boot.tmp.cleanOnBoot = true;
  nix.settings.auto-optimise-store = true;
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';
  #
  # Automount
  #
  services.gvfs.enable = true;
  services.udisks2.enable = true;
  #
  # Networking
  #
  networking.firewall.enable = true;
  services.fail2ban.enable = true;
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
    settings.PermitRootLogin = "no";
  };
}
