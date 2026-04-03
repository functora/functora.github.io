{
  pkgs,
  name,
  text,
  home ? null,
  user ? "bubble",
  pasta ? true,
  network ? false,
}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    inherit name text;
  };
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "${user}:x:1000:1000:${user}:${
      if home == null
      then "/tmp"
      else "/home/${user}"
    }:/bin/sh";
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      app.package = app;
      gpu.enable = true;
      fonts.enable = true;
      pasta.enable = pasta;
      locale.enable = true;
      etc.sslCertificates.enable = network;
      bubblewrap = {
        inherit network;
        dieWithParent = true;
        sockets.pulse = true;
        sockets.wayland = true;
        bind.ro = [
          [(toString passwd) "/etc/passwd"]
        ];
        bind.rw =
          if home == null
          then []
          else [
            [
              (sloth.mkdir (sloth.concat' sloth.homeDir "/doom"))
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
