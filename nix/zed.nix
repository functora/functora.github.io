{pkgs ? import <nixpkgs> {}}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "zed";
    text = "${pkgs.zed-editor}/bin/zeditor";
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      app.package = app;
      gpu.enable = true;
      gpu.provider = "bundle";
      fonts.enable = true;
      locale.enable = true;
      etc.sslCertificates.enable = true;

      bubblewrap = {
        network = true;
        sockets.pulse = true;
        sockets.wayland = true;
        env.RUST_BACKTRACE = "full";
        bindEntireStore = true;

        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/zed"))
            (sloth.concat' sloth.homeDir "/zed")
          ]
        ];

        bind.ro = [
          "/bin/sh"
          "/etc/group"
          "/etc/passwd"
          "/run/current-system/sw/bin/bash"
          (sloth.concat' sloth.homeDir "/.nix-profile")
          [
            (toString (import ./zed-config.nix {inherit pkgs;}))
            (sloth.concat' sloth.homeDir "/.config/zed/settings.json")
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
