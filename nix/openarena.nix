let
  pkgs = import ./nixpkgs.nix;
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "openarena";
    text = ''
      ${pkgs.openarena}/bin/openarena \
        +set fs_homepath ~/.openarena \
        +set fs_game omega "$@"
    '';
  };
  rat = pkgs.fetchurl {
    url = "https://github.com/rdntcntrl/ratarena_release/releases/download/v0.18.2/z-ratmod-v0.18.2.pk3";
    hash = "sha256-CvO2696RgeM/ovI4OSIiGqtljk9YwGDgLHhkLbfB+oU=";
  };
  omega = pkgs.fetchurl {
    url = "https://github.com/Bishop-333/OmegA-mod/releases/download/v3.3.8/z_omega-v3.3.8.pk3";
    hash = "sha256-2/PLMtJy0RJ/TonKtJASgXammaC8j6kAlOUZgAsgdBI=";
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      app.package = app;
      gpu.enable = true;
      fonts.enable = true;
      locale.enable = true;
      etc.sslCertificates.enable = true;
      bubblewrap = {
        network = false;
        sockets.pulse = true;
        sockets.wayland = true;
        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/openarena/.openarena/rat"))
            (sloth.concat' sloth.homeDir "/.openarena/rat")
          ]
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/openarena/.openarena/omega"))
            (sloth.concat' sloth.homeDir "/.openarena/omega")
          ]
        ];
        bind.ro = [
          [
            (toString ../cfg/q3.cfg)
            (
              sloth.concat'
              sloth.homeDir
              "/.openarena/rat/autoexec.cfg"
            )
          ]
          [
            (toString ../cfg/q3.cfg)
            (
              sloth.concat'
              sloth.homeDir
              "/.openarena/omega/autoexec.cfg"
            )
          ]
          [
            (toString rat)
            (
              sloth.concat'
              sloth.homeDir
              "/.openarena/rat/z-ratmod-v0.18.2.pk3"
            )
          ]
          [
            (toString omega)
            (
              sloth.concat'
              sloth.homeDir
              "/.openarena/omega/z_omega-v3.3.8.pk3"
            )
          ]
        ];
        tmpfs = [
          "/tmp"
        ];
      };
    };
  };
in
  sandbox.config.env
