# Sandboxed opencode environment with tailscale-only connectivity.
#
# services.functora.vibeServer: headless "vibe-server" sandbox running
#   `opencode serve` as a systemd user service, also joining the tailnet as
#   the server node.
# services.functora.vibeClient: interactive "vibe-client" sandbox (alacritty +
#   tmux) whose `opencode` binary is a thin client attaching to the server
#   node.
#
# Both variants run their own tailscaled in userspace-networking mode inside
# their own network namespace, so no port is forwarded into or out of the host
# network and the sandbox restrictions (pasta isolate) stay unchanged.
{
  config,
  lib,
  pkgs,
  ...
}: let
  unst = import ./nixpkgs-unstable.nix;
  agentsMd = ../AGENTS.md;

  loopbackAddress = "127.0.0.1";
  opencodePort = 4096;
  tailscaleProxyPort = 1055;
  tailscaleProxyListenAddress = "${loopbackAddress}:${toString tailscaleProxyPort}";
  tailscaleProxyUrl = "http://${loopbackAddress}:${toString tailscaleProxyPort}";
  noProxyHosts = "localhost,127.0.0.1,::1";

  sleepBin = "${pkgs.coreutils}/bin/sleep";
  waitAttempts = 100;
  waitIntervalSeconds = "0.1";

  secretsRelativePath = ".config/vibe/secrets";
  secretsDir = "$HOME/${secretsRelativePath}";
  serverAuthKeyFile = "${secretsDir}/TS_SERVER_AUTHKEY";
  clientAuthKeyFile = "${secretsDir}/TS_CLIENT_AUTHKEY";
  opencodePasswordFile = "${secretsDir}/OPENCODE_SERVER_PASSWORD";
  tailscaledSocketName = "tailscaled.sock";

  requireSecretFile = secretPath: ''
    if [ ! -r "${secretPath}" ] || [ ! -s "${secretPath}" ]; then
      echo "vibe: missing, unreadable, or empty secret file: ${secretPath}" >&2
      exit 1
    fi
  '';

  secretPreflight = authKeyFile:
    requireSecretFile authKeyFile + requireSecretFile opencodePasswordFile;

  opencodePasswordScript = ''
    ${requireSecretFile opencodePasswordFile}
    unset OPENCODE_PASSWORD OPENCODE_SERVER_PASSWORD
    opencode_password="$(<"${opencodePasswordFile}")"
    if [ -z "$opencode_password" ]; then
      echo "vibe: empty secret file: ${opencodePasswordFile}" >&2
      exit 1
    fi
    export OPENCODE_SERVER_PASSWORD="$opencode_password"
  '';

  mkTailscaledScript = {
    authKeyFile,
    stateDir,
    hostname,
    upInBackground,
  }: let
    backgroundSuffix =
      if upInBackground
      then " &"
      else "";
  in ''
    tailscale_state_dir="${stateDir}"
    ${pkgs.coreutils}/bin/mkdir -p "$tailscale_state_dir"
    tailscaled_socket="$tailscale_state_dir/${tailscaledSocketName}"

    tailscaled \
      --tun=userspace-networking \
      --statedir="$tailscale_state_dir" \
      --socket="$tailscaled_socket" \
      --socks5-server=${tailscaleProxyListenAddress} \
      --outbound-http-proxy-listen=${tailscaleProxyListenAddress} &
    tailscaled_pid=$!

    cleanup() {
      kill "$tailscaled_pid" 2>/dev/null || true
    }
    trap cleanup EXIT
    trap 'cleanup; exit 143' TERM INT

    for ((attempt = 0; attempt < ${toString waitAttempts}; attempt++)); do
      if [ -S "$tailscaled_socket" ]; then
        break
      fi
      ${sleepBin} ${waitIntervalSeconds}
    done

    tailscale_auth_key="$(<"${authKeyFile}")"
    if [ -z "$tailscale_auth_key" ]; then
      echo "vibe: empty secret file: ${authKeyFile}" >&2
      exit 1
    fi
    tailscale --socket="$tailscaled_socket" up --auth-key="$tailscale_auth_key" --hostname="${hostname}" --accept-dns=false${backgroundSuffix}
  '';

  mkRuntimeInputs = server:
    with pkgs; [
      (import ./../pub/vi/nix/default.nix {ai = true;})
      fontconfig
      busybox
      curl
      wget
      git
      ripgrep
      alacritty
      xdg-utils
      tmux
      nix
      direnv
      nix-direnv
      asciinema
      asciinema-agg
      tailscale
      (
        if server
        then unst.opencode
        else opencodeClient
      )
      (writeShellApplication {
        name = "notify-user";
        text = ''
          for _ in 1 2 3; do
            ${mpv}/bin/mpv \
              "${sound-theme-freedesktop}/share/sounds/freedesktop/stereo/complete.oga"
          done
        '';
      })
      #
      # Web
      #
      chromium
      playwright
      playwright-test
      cloudflared
      #
      # Scripting
      #
      nodejs
      python3Minimal
      #
      # Formatter
      #
      stylua
      sleek
      prettier
      html-tidy
      alejandra
      topiary
      taplo
    ];

  serverHostname = "vibe-server";
  serverStateDir = "$HOME/.local/state/tailscale/server";

  serverProxyEnvClear = ''
    unset HTTP_PROXY HTTPS_PROXY ALL_PROXY http_proxy https_proxy all_proxy NO_PROXY no_proxy
  '';

  serverScript =
    secretPreflight serverAuthKeyFile
    + serverProxyEnvClear
    + mkTailscaledScript {
      authKeyFile = serverAuthKeyFile;
      stateDir = serverStateDir;
      hostname = serverHostname;
      upInBackground = false;
    }
    + ''

      ${opencodePasswordScript}

      opencode serve --port ${toString opencodePort} --hostname ${loopbackAddress}
    '';

  clientHostname = "vibe-client";
  clientInstanceId = "\${VIBE_INSTANCE_ID:?vibe: VIBE_INSTANCE_ID is not set}";
  clientStateDir = "/tmp/vibe-ts-${clientInstanceId}";
  clientSocketPath = "${clientStateDir}/${tailscaledSocketName}";

  clientScript =
    secretPreflight clientAuthKeyFile
    + mkTailscaledScript {
      authKeyFile = clientAuthKeyFile;
      stateDir = clientStateDir;
      hostname = "${clientHostname}-${clientInstanceId}";
      upInBackground = true;
    }
    + ''

      alacritty
    '';

  opencodeClient = pkgs.writeShellApplication {
    name = "opencode";
    runtimeInputs = [pkgs.tailscale];
    text = ''
      ${opencodePasswordScript}

      tailscaled_socket="${clientSocketPath}"

      for ((attempt = 0; attempt < ${toString waitAttempts}; attempt++)); do
        if tailscale --socket="$tailscaled_socket" ip -4 >/dev/null 2>&1; then
          break
        fi
        ${sleepBin} ${waitIntervalSeconds}
      done

      if ! tailscale --socket="$tailscaled_socket" ip -4 >/dev/null 2>&1; then
        echo "vibe: tailscale is not connected; tailscale status:" >&2
        tailscale --socket="$tailscaled_socket" status >&2 || true
        exit 1
      fi

      export HTTP_PROXY="${tailscaleProxyUrl}"
      export ALL_PROXY="${tailscaleProxyUrl}"
      export HTTPS_PROXY="${tailscaleProxyUrl}"
      export http_proxy="${tailscaleProxyUrl}"
      export all_proxy="${tailscaleProxyUrl}"
      export https_proxy="${tailscaleProxyUrl}"
      export NO_PROXY="${noProxyHosts}"
      export no_proxy="${noProxyHosts}"

      exec ${unst.opencode}/bin/opencode --server "http://${serverHostname}:${toString opencodePort}" "$@"
    '';
  };

  mkRoBinds = sloth:
    map (rel: let
      sub =
        if lib.hasPrefix "/" rel
        then rel
        else "/${rel}";
    in [
      (sloth.concat' sloth.homeDir "/vibe${sub}")
      (sloth.concat' sloth.homeDir sub)
    ])
    config.services.functora.vibeRelativeRo;

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
        ++ mkRoBinds sloth
        ++ [
          "/bin/sh"
          "/usr/bin/env"
          "/run/current-system/sw/bin/bash"
          "/run/current-system/sw/bin/less"
          ["${pkgs.bash}/bin/bash" "/bin/bash"]
          (sloth.concat' sloth.homeDir "/.bashrc")
          (sloth.concat' sloth.homeDir "/.bash_profile")
          (sloth.concat' sloth.homeDir "/${secretsRelativePath}")
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
          VIBE_INSTANCE_ID = sloth.instanceId;
          NIX_CONFIG = "experimental-features = nix-command flakes";
          NIXPKGS_ALLOW_UNFREE = "1";
          ANDROID_USER_HOME = "~/.android";
          GRADLE_USER_HOME = "~/.gradle";
        };
    }
  ];

  mkVibeSandbox = {
    name,
    script,
    server,
  }:
    import ./bubble.nix {
      inherit pkgs name;
      text = script;
      runtimeInputs = mkRuntimeInputs server;
      user = "vibe";
      network = true;
      home = "vibe";
      inherit mkOverlay;
    };

  clientSandbox = mkVibeSandbox {
    name = clientHostname;
    script = clientScript;
    server = false;
  };

  serverSandbox = mkVibeSandbox {
    name = serverHostname;
    script = serverScript;
    server = true;
  };
in {
  options.services.functora.vibeServer = {
    enable = lib.mkEnableOption "sandboxed opencode vibe-server on the tailnet";
    package = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = ''
        Sandbox derivation providing bin/vibe-server. Owned by this
        module and immutable: any attempt to set it fails evaluation.
      '';
    };
  };

  options.services.functora.vibeClient = {
    package = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = ''
        Sandbox derivation providing bin/vibe-client. Owned by this
        module and immutable: any attempt to set it fails evaluation.
      '';
    };
  };

  options.services.functora.vibeRelativeRo = lib.mkOption {
    type = lib.types.listOf (lib.types.mkOptionType {
      name = "vibeMountPath";
      description = "path relative to ~/vibe without a \"..\" component";
      check = rel: lib.isString rel && (! lib.elem ".." (lib.splitString "/" rel));
    });
    default = [];
    description = ''
      Paths (relative to ~/vibe on the host and mirrored at $HOME inside both
      sandboxes) that are mounted read-only at launch. A leading slash is
      optional, ".." path components are rejected, and missing paths are
      silently skipped.
    '';
    example = ["/alpha/.git" "/beta.git" "/gamma/docs"];
  };

  config = lib.mkMerge [
    {
      services.functora.vibeServer.package = serverSandbox;
      services.functora.vibeClient.package = clientSandbox;
    }
    (lib.mkIf config.services.functora.vibeServer.enable {
      systemd.user.services.vibe-server = {
        wantedBy = ["default.target"];
        serviceConfig = {
          ExecStart = "${config.services.functora.vibeServer.package}/bin/vibe-server";
          Restart = "on-failure";
          RestartSec = 3;
          NoNewPrivileges = true;
          RestrictSUIDSGID = true;
          LockPersonality = true;
          RestrictRealtime = true;
        };
      };
    })
  ];
}
