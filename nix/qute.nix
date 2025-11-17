{pkgs}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "qute";
    text = ''
      ${pkgs.qutebrowser}/bin/qutebrowser "$@"
    '';
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
        bind.ro = [
          [
            (toString ../cfg/qutebrowser.py)
            (
              sloth.concat'
              sloth.homeDir
              ".config/qutebrowser/config.py"
            )
          ]
        ];
        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/qute"))
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
