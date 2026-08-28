{pkgs ? import <nixpkgs> {}}: let
  agentsMd = ../AGENTS.md;
  unst = import ./nixpkgs-unstable.nix;
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
      ripgrep
      alacritty
      xdg-utils
      python3Minimal
      tmux
      nix
      direnv
      nix-direnv
      asciinema
      asciinema-agg
      (writeShellApplication {
        name = "notify-user";
        text = ''
          for _ in $(seq 3); do
            ${mpv}/bin/mpv \
              "${sound-theme-freedesktop}/share/sounds/freedesktop/stereo/complete.oga"
          done
        '';
      })
    ];
  };
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "vibe:x:1000:1000:vibe:/tmp:/bin/sh";
  };
  codexToml = pkgs.writeTextFile {
    name = "codex";
    text = ''
      profile = "free"
      sandbox_mode = "danger-full-access"

      [features]
      unified_exec = true
      streamable_shell = true
      apply_patch_freeform = true
      web_search_request = true
      ghost_commit = true

      [profiles.free]
      model = "arcee-ai/trinity-large-preview:free"
      model_provider = "openrouter"

      [profiles.lite]
      model = "gemini-2.5-flash-lite"
      model_provider = "gemini"

      [profiles.std]
      model = "gemini-2.5-flash"
      model_provider = "gemini"

      [profiles.pro]
      model = "gemini-2.5-pro"
      model_provider = "gemini"

      [model_providers.openrouter]
      name = "openrouter"
      base_url = "https://openrouter.ai/api/v1"
      env_key = "OPENROUTER_API_KEY"

      [model_providers.gemini]
      name = "gemini"
      base_url = "https://generativelanguage.googleapis.com/v1beta/openai"
      env_key = "GEMINI_API_KEY"
    '';
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
        ++ [
          pkgs.dejavu_fonts
          pkgs.noto-fonts-color-emoji
        ];
      pasta.enable = true;
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
          [(toString passwd) "/etc/passwd"]
          ["${pkgs.bash}/bin/bash" "/bin/bash"]
          (sloth.concat' sloth.homeDir "/.bashrc")
          (sloth.concat' sloth.homeDir "/.bash_profile")
          (sloth.concat' sloth.homeDir "/.config/tmux")
          (sloth.concat' sloth.homeDir "/.config/alacritty")
          (sloth.concat' sloth.homeDir "/.config/nvim/coc-settings.json")
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.qwen/QWEN.md")
          ]
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.codex/AGENTS.md")
          ]
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.gemini/GEMINI.md")
          ]
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.claude/CLAUDE.md")
          ]
          [
            (toString codexToml)
            (sloth.concat' sloth.homeDir "/.codex/config.toml")
          ]
          [
            (toString ../cfg/opencode.json)
            (sloth.concat' sloth.homeDir "/.config/opencode/opencode.jsonc")
          ]
          [
            (toString ../cfg/opencode-notify.ts)
            (sloth.concat' sloth.homeDir "/.config/opencode/notify.ts")
          ]
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
        env.NIXPKGS_ALLOW_UNFREE = "1";
        env.ANDROID_USER_HOME = "~/.android";
        env.GRADLE_USER_HOME = "~/.gradle";
      };
    };
  };
in
  sandbox.config.env
