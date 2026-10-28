{pkgs ? import <nixpkgs> {}}: let
  unst = import ./nixpkgs-unstable.nix;
  agentsMd = ../AGENTS.md;
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
    unst.opencode
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
in
  import ./bubble.nix {
    inherit pkgs;
    name = "vibe";
    text = "alacritty";
    inherit runtimeInputs;
    user = "vibe";
    network = true;
    home = "vibe";
    mkOverlay = sloth: [
      {
        path = ["fonts" "fonts"];
        update = _:
          builtins.filter pkgs.lib.attrsets.isDerivation (
            builtins.attrValues pkgs.nerd-fonts
          )
          ++ [
            pkgs.dejavu_fonts
            pkgs.noto-fonts-color-emoji
          ];
      }
      {
        path = ["bubblewrap" "bind" "ro"];
        update = prev:
          prev
          ++ [
            "/bin/sh"
            "/usr/bin/env"
            "/run/current-system/sw/bin/bash"
            "/run/current-system/sw/bin/less"
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
              (toString ../cfg/opencode.json)
              (sloth.concat' sloth.homeDir "/.config/opencode/opencode.jsonc")
            ]
            [
              (toString ../cfg/opencode-notify)
              (sloth.concat' sloth.homeDir "/.config/opencode/notify")
            ]
          ];
      }
      {
        path = ["bubblewrap" "env"];
        update = prev:
          prev
          // {
            NIX_CONFIG = "experimental-features = nix-command flakes";
            NIXPKGS_ALLOW_UNFREE = "1";
            ANDROID_USER_HOME = "~/.android";
            GRADLE_USER_HOME = "~/.gradle";
          };
      }
    ];
  }
