{pkgs ? import <nixpkgs> {}}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "vibe";
    text = "alacritty";
    runtimeInputs = with pkgs; [
      (import ./../pub/vi/nix/default.nix {ai = true;})
      fontconfig
      busybox
      curl
      wget
      git
      alacritty
      tmux
      nix
    ];
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      app.package = app;
      gpu.enable = true;
      gpu.provider = "bundle";
      fonts.enable = true;
      fonts.fonts =
        builtins.filter pkgs.lib.attrsets.isDerivation (
          builtins.attrValues pkgs.nerd-fonts
        )
        ++ [pkgs.dejavu_fonts];
      locale.enable = true;
      etc.sslCertificates.enable = true;
      bubblewrap = {
        network = true;
        sockets.pulse = true;
        sockets.wayland = true;
        bind.ro = [
          "/bin/sh"
          "/usr/bin/env"
          "/run/current-system/sw/bin/bash"
          "/run/current-system/sw/bin/less"
          (sloth.concat' sloth.homeDir "/.config/tmux")
          (sloth.concat' sloth.homeDir "/.config/alacritty")
          (sloth.concat' sloth.homeDir "/.config/nvim/coc-settings.json")
        ];
        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/vibe"))
            sloth.homeDir
          ]
        ];
        tmpfs = [
          "/tmp"
        ];
        env.NIX_CONFIG = "experimental-features = nix-command flakes";
      };
    };
  };
in
  sandbox.config.env
