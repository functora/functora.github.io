{
  pkgs,
  name,
  text,
  runtimeInputs ? [],
  home ? null,
  user ? "bubble",
  pasta ? true,
  network ? false,
  mkExtend ? _: {}, # Sloth -> AttrSet,
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
  sandboxHome =
    if home == null
    then "/tmp"
    else "/home/${user}";
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "${user}:x:1000:1000:${user}:${sandboxHome}:/bin/sh";
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
      pkgs.lib.updateManyAttrsByPath (mkOverlay sloth) (
        {
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
            env = {
              USER = user;
              LOGNAME = user;
              HOME = sloth.homeDir;
              XDG_DATA_HOME = sloth.concat' sloth.homeDir "/.local/share";
              XDG_CACHE_HOME = sloth.concat' sloth.homeDir "/.cache";
              XDG_STATE_HOME = sloth.concat' sloth.homeDir "/.local/state";
              XDG_CONFIG_HOME = sloth.concat' sloth.homeDir "/.config";
              ALSA_PLUGIN_DIR = "${pkgs.alsa-plugins}/lib/alsa-lib";
            };
            bind.ro = [
              [(toString passwd) "/etc/passwd"]
              [(toString asound) "/etc/asound.conf"]
              ["${pkgs.mesa_i686}" "/run/opengl-driver-32"]
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
            tmpfs =
              if home == null
              then ["/tmp" sloth.homeDir]
              else ["/tmp"];
          };
        }
        // mkExtend sloth
      );
  };
in
  sandbox.config.env
