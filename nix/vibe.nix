# Sandboxed opencode environment with tailscale-only connectivity.
#
# server = false: interactive "vibe-client" sandbox (alacritty + tmux) whose
#   `opencode` binary is a thin client attaching to the server node.
# server = true: headless "vibe-server" sandbox running `opencode serve` as a
#   systemd user service, also joining the tailnet as the server node.
#
# Both variants run their own tailscaled in userspace-networking mode inside
# their own network namespace, so no port is forwarded into or out of the host
# network and the sandbox restrictions (pasta isolate) stay unchanged.
{
  pkgs ? import <nixpkgs> {},
  server ? false,
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
  tailscaleAuthKeyFile = "${secretsDir}/TS_AUTHKEY";
  opencodePasswordFile = "${secretsDir}/OPENCODE_SERVER_PASSWORD";
  tailscaledSocketName = "tailscaled.sock";

  requireSecretFile = secretPath: ''
    if [ ! -r "${secretPath}" ] || [ ! -s "${secretPath}" ]; then
      echo "vibe: missing, unreadable, or empty secret file: ${secretPath}" >&2
      exit 1
    fi
  '';

  secretPreflight = requireSecretFile tailscaleAuthKeyFile + requireSecretFile opencodePasswordFile;

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
    stateDir,
    hostname,
    ephemeral,
    upInBackground,
  }: let
    ephemeralFlag =
      if ephemeral
      then "--ephemeral "
      else "";
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

    tailscale_auth_key="$(<"${tailscaleAuthKeyFile}")"
    if [ -z "$tailscale_auth_key" ]; then
      echo "vibe: empty secret file: ${tailscaleAuthKeyFile}" >&2
      exit 1
    fi
    tailscale --socket="$tailscaled_socket" up --auth-key="$tailscale_auth_key" ${ephemeralFlag}--hostname="${hostname}" --accept-dns=false${backgroundSuffix}
  '';

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

  #
  # Server
  #

  serverHostname = "vibe-server";
  serverStateDir = "$HOME/.local/state/tailscale/server";

  serverProxyEnvClear = ''
    unset HTTP_PROXY HTTPS_PROXY ALL_PROXY http_proxy https_proxy all_proxy NO_PROXY no_proxy
  '';

  serverScript =
    secretPreflight
    + serverProxyEnvClear
    + mkTailscaledScript {
      stateDir = serverStateDir;
      hostname = serverHostname;
      ephemeral = false;
      upInBackground = false;
    }
    + ''

      ${opencodePasswordScript}

      opencode serve --port ${toString opencodePort} --hostname ${loopbackAddress}
    '';

  #
  # Client
  #

  clientHostname = "vibe-client";
  clientInstanceId = "\${VIBE_INSTANCE_ID:?vibe: VIBE_INSTANCE_ID is not set}";
  clientStateDir = "/tmp/vibe-ts-${clientInstanceId}";
  clientSocketPath = "${clientStateDir}/${tailscaledSocketName}";

  clientScript =
    secretPreflight
    + mkTailscaledScript {
      stateDir = clientStateDir;
      hostname = "${clientHostname}-${clientInstanceId}";
      ephemeral = true;
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
        echo "vibe: tailscale is not connected" >&2
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
in
  import ./bubble.nix {
    inherit pkgs;
    name =
      if server
      then serverHostname
      else clientHostname;
    text =
      if server
      then serverScript
      else clientScript;
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
  }
