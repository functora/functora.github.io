{pkgs ? import <nixpkgs> {}}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "shotcut";
    text = ''${pkgs.shotcut}/bin/shotcut "$@"'';
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      app.package = app;
      gpu.enable = true;
      fonts.enable = true;
      locale.enable = true;
      bubblewrap = {
        network = false;
        sockets.pulse = true;
        sockets.wayland = true;
        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/vids"))
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
