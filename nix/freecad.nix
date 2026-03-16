{pkgs ? import <nixpkgs> {}}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "freecad";
    text = "${pkgs.freecad}/bin/freecad";
  };
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "vibe:x:1000:1000:vibe:/tmp:/bin/sh";
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
        bind.ro = [
          [(toString passwd) "/etc/passwd"]
        ];
        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/prusa"))
            sloth.homeDir
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
