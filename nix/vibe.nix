{pkgs ? import <nixpkgs> {}}: let
  unst = import ./nixpkgs-unstable.nix;
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  gemini = pkgs.writeShellApplication {
    name = "gemini";
    text = ''
      ${unst.gemini-cli}/bin/gemini \
        --model gemini-2.5-flash-lite \
        --yolo \
        "$@"
    '';
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
      gomplate
      qutebrowser
      unst.codex
      gemini
      (pkgs.writeShellApplication {
        name = "qwen";
        text = ''${unst.qwen-code}/bin/qwen -y "$@"'';
      })
    ];
  };
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "vibe:x:1000:1000:vibe:/tmp:/bin/sh";
  };
  agentsMd = pkgs.writeTextFile {
    name = "AGENTS.md";
    text = ''
      Read the `README.md` file if available. Study the project source code. Write your code in a strictly functional style while ensuring high efficiency. Examples of good code can be found in the latest versions of the crates `functora-tagged` and `functora`, study their source code and use a similar style. Avoid mutable variables (`mut`), imperative constructs (e.g., `for`, `while`, `loop`, or `return`), and instead use functional iterators and control flow with `?` operator. Eliminate redundant closures and variables. Maximize method chaining, always favor chaining and pipelining over temporary one-time variables. Use `?` instead of `.map_err(..)` where possible. Avoid `.clone()`, unnecessary allocations, and extra dependencies. Prefer the standard library exclusively when possible. Follow the existing project code style. Use meaningful, context-clear identifiers with a preference for brevity, as in the original code. Do not include comments; the code must be self-explanatory. Do not nest code blocks too much, avoid nesting brackets like `(.. (..) ..)` or `{.. { .. } ..}`, the code must be as flat as possible, deal with complexity using control flow `?` operator, chaining and piping methods. To check Rust code use the command `cargo clippy --all-features`. The code should not have any warnings or errors. To test Rust code use the command `cargo test --all-features`. To get Rust test coverage report use the command `cargo tarpaulin --all-features --engine llvm -o Lcov`, the report is written into `lcov.info` file. Test coverage should be as high as possible. All tests should be located in `./tests` directory. The final step is to format the code with `cargo fmt`.
    '';
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
  opencodeJson = pkgs.writeTextFile {
    name = "opencode";
    text = ''
      {
        "$schema": "https://opencode.ai/config.json",
        "model": "llama-cpp/self-hosted",
        "provider": {
          "llama-cpp": {
            "npm": "@ai-sdk/openai-compatible",
            "name": "llama-cpp",
            "options": {
              "baseURL": "http://localhost:11434"
            },
            "models": {
              "self-hosted": {
                "name": "self-hosted"
              }
            }
          }
        }
      }
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
            (toString opencodeJson)
            (sloth.concat' sloth.homeDir "/.config/opencode/opencode.jsonc")
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
      };
    };
  };
in
  sandbox.config.env
