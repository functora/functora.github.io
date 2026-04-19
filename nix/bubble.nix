{
  pkgs,
  name,
  text,
  runtimeInputs ? [],
  home ? null,
  user ? "bubble",
  pasta ? true,
  network ? false,
  mkOverlay ? _: [], # Sloth -> [{ path :: [String]; update :: (Any -> Any); }]
}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    inherit name text runtimeInputs;
  };
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "${user}:x:1000:1000:${user}:${
      if home == null
      then "/tmp"
      else "/home/${user}"
    }:/bin/sh";
  };
  asound = pkgs.writeText "asound.conf" ''
    pcm.!default {
      type pulse
    }
    ctl.!default {
      type pulse
    }
  '';
  sandbox = mkNixPak {
    config = {sloth, ...}:
      pkgs.lib.updateManyAttrsByPath (mkOverlay sloth) {
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
          env.ALSA_PLUGIN_DIR = "${pkgs.alsa-plugins}/lib/alsa-lib";
          bind.ro = [
            [(toString passwd) "/etc/passwd"]
            [(toString asound) "/etc/asound.conf"]
          ];
          bind.rw =
            if home == null
            then []
            else [
              [
                (sloth.mkdir (sloth.concat' sloth.homeDir "/${home}"))
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
