{pkgs ? import <nixpkgs> {}}: let
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
      python3Minimal
      tmux
      nix
      direnv
      nix-direnv
      asciinema
      asciinema-agg
      unst.codex
      unst.gemini-cli
    ];
  };
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "vibe:x:1000:1000:vibe:/tmp:/bin/sh";
  };
  agentsMd = pkgs.writeTextFile {
    name = "AGENTS.md";
    text = ''
      Read the `README.md` file if available. Study the project source code. Write your code in a strictly functional style while ensuring high efficiency. Avoid mutable variables (`mut`), imperative constructs (e.g., `for`, `while`, `loop`, or `return`), and instead use functional iterators. Eliminate redundant closures and variables. Maximize method and function chaining. Avoid `.clone()`, unnecessary allocations, and extra dependencies. Prefer the standard library exclusively when possible. Follow the existing project code style. Use meaningful, context-clear identifiers with a preference for brevity, as in the original code. Do not include comments; the code must be self-explanatory.
    '';
  };
  codexToml = pkgs.writeTextFile {
    name = "codex";
    text = ''
      profile = "lite"
      sandbox_mode = "danger-full-access"

      [features]
      unified_exec = true
      streamable_shell = true
      apply_patch_freeform = true
      web_search_request = true
      ghost_commit = true

      [profiles.lite]
      model = "gemini-2.5-flash-lite"
      model_provider = "gemini"

      [profiles.std]
      model = "gemini-2.5-flash"
      model_provider = "gemini"

      [profiles.pro]
      model = "gemini-2.5-pro"
      model_provider = "gemini"

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
          [(toString passwd) "/etc/passwd"]
          ["${pkgs.bash}/bin/bash" "/bin/bash"]
          (sloth.concat' sloth.homeDir "/.bashrc")
          (sloth.concat' sloth.homeDir "/.bash_profile")
          (sloth.concat' sloth.homeDir "/.config/tmux")
          (sloth.concat' sloth.homeDir "/.config/alacritty")
          (sloth.concat' sloth.homeDir "/.config/nvim/coc-settings.json")
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.codex/AGENTS.md")
          ]
          [
            (toString codexToml)
            (sloth.concat' sloth.homeDir "/.codex/config.toml")
          ]
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.gemini/GEMINI.md")
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
      };
    };
  };
in
  sandbox.config.env
