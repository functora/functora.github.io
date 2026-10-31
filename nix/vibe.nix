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

  #
  # opencode-ntfy.sh plugin (OpenCode V2 API). Pinned to a GitHub main commit
  # that was migrated to the V2 plugin API. The npm registry still serves the
  # V1 plugin (opencode-ntfy.sh@1.1.0) which opencode v2 rejects, so we fetch
  # and build the plugin deterministically with a pinned rev and hash.
  #
  ntfyPluginVersion = "1.2.3";
  ntfyPluginRev = "01ace501a408bca5318453fa6cdf89dcbfd6a217";
  ntfyPluginHash = "sha256-ES4uR9nLRCwyi46kZXklS3kBSxOaDMGPuGGGDHKNphM=";

  ntfyPluginSrc = pkgs.fetchFromGitHub {
    owner = "lannuttia";
    repo = "opencode-ntfy.sh";
    rev = ntfyPluginRev;
    hash = ntfyPluginHash;
    fetchSubmodules = false;
  };

  ntfySdkSrc = pkgs.fetchurl {
    url = "https://registry.npmjs.org/opencode-notification-sdk/-/opencode-notification-sdk-2.0.1.tgz";
    sha256 = "1g648rjngvk9f5wiiwxmhhzlq05j6p0297dq7na6k9bz15a2xwxm";
  };

  ntfyIsoSrc = pkgs.fetchurl {
    url = "https://registry.npmjs.org/iso8601-duration/-/iso8601-duration-2.1.3.tgz";
    sha256 = "116rxjqw3hh9xyw9gki7dis2i3v5gwm1621wg5khh22rvxlgb3fq";
  };

  ntfyNodeTypesSrc = pkgs.fetchurl {
    url = "https://registry.npmjs.org/@types/node/-/node-25.2.3.tgz";
    sha256 = "0h4vwj599h7fwivdj6icmwidbss0c240q37qhpj0z1lif2iqs2qq";
  };

  #
  # Build the opencode-ntfy.sh plugin directory for opencode v2.
  #
  # Layout:
  #   package.json        name + main (no "exports": Bun <1.2 can not
  #                       self-resolve a named package, so we fall back to main)
  #   index.js            shim re-exporting ./dist/index.js
  #   dist/               compiled TypeScript
  #   notification-ntfy.schema.json
  #   node_modules/       runtime deps (opencode-notification-sdk,
  #                       iso8601-duration). @opencode/plugin is provided by
  #                       the opencode host runtime and must not be vendored.
  #
  ntfyPlugin = pkgs.stdenv.mkDerivation {
    name = "opencode-ntfy.sh-${ntfyPluginVersion}";
    src = ntfyPluginSrc;
    nativeBuildInputs = [pkgs.typescript pkgs.nodejs];
    doCheck = false;

    buildPhase = ''
      mkdir -p node_modules/@types/node
      mkdir -p node_modules/opencode-notification-sdk
      mkdir -p node_modules/iso8601-duration

      ${pkgs.gnutar}/bin/tar -xzf ${ntfyNodeTypesSrc} --strip-components=1 \
        -C node_modules/@types/node
      ${pkgs.gnutar}/bin/tar -xzf ${ntfySdkSrc} --strip-components=1 \
        -C node_modules/opencode-notification-sdk
      ${pkgs.gnutar}/bin/tar -xzf ${ntfyIsoSrc} --strip-components=1 \
        -C node_modules/iso8601-duration

      ${pkgs.gnused}/bin/sed -i 's/"moduleResolution": "bundler"/"moduleResolution": "bundler", "types": ["node"]/' tsconfig.json
      ${pkgs.typescript}/bin/tsc
    '';

    installPhase = ''
      mkdir -p $out/dist $out/node_modules
      cp -r dist/* $out/dist
      cp package.json $out/package.json
      cp notification-ntfy.schema.json $out/notification-ntfy.schema.json
      printf 'export { default } from "./dist/index.js"\n' > $out/index.js
      cp -r node_modules $out/node_modules
    '';
  };

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
  ntfyTopicFile = "${secretsDir}/NTFY_TOPIC";
  tailscaledSocketName = "tailscaled.sock";

  requireSecretFile = secretPath: ''
    if [ ! -r "${secretPath}" ] || [ ! -s "${secretPath}" ]; then
      echo "vibe: missing, unreadable, or empty secret file: ${secretPath}" >&2
      exit 1
    fi
  '';

  secretPreflight = authKeyFile:
    requireSecretFile authKeyFile + requireSecretFile opencodePasswordFile;

  ntfyEnabled = config.services.functora.vibeServer.enable && config.services.functora.vibeNtfy.enable;

  ntfyPreflight = lib.optionalString ntfyEnabled (requireSecretFile ntfyTopicFile);

  ntfyNotificationJson = pkgs.writeText "notification-ntfy.json" (builtins.toJSON {
    backend = {
      server = "https://ntfy.sh";
      topic = "{file:~/${secretsRelativePath}/NTFY_TOPIC}";
    };
  });

  ntfyPluginConfigJson = pkgs.writeText "opencode-ntfy.json" (builtins.toJSON {
    plugins = [ntfyPlugin];
  });

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
      #
      # Notifications
      #
      ntfyPlugin
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

  proxyExports = ''
    export HTTP_PROXY="${tailscaleProxyUrl}"
    export ALL_PROXY="${tailscaleProxyUrl}"
    export HTTPS_PROXY="${tailscaleProxyUrl}"
    export http_proxy="${tailscaleProxyUrl}"
    export all_proxy="${tailscaleProxyUrl}"
    export https_proxy="${tailscaleProxyUrl}"
    export NO_PROXY="${noProxyHosts}"
    export no_proxy="${noProxyHosts}"
  '';

  ntfyEgress = lib.optionalString ntfyEnabled proxyExports;

  serverScript =
    secretPreflight serverAuthKeyFile
    + ntfyPreflight
    + serverProxyEnvClear
    + mkTailscaledScript {
      authKeyFile = serverAuthKeyFile;
      stateDir = serverStateDir;
      hostname = serverHostname;
      upInBackground = false;
    }
    + ntfyEgress
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
    + ntfyPreflight
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

      ${proxyExports}
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
        ]
        ++ lib.optionals ntfyEnabled [
          [
            (toString ntfyNotificationJson)
            (sloth.concat' sloth.homeDir "/.config/opencode/notification-ntfy.json")
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
        }
        // lib.optionalAttrs ntfyEnabled {
          OPENCODE_CONFIG = toString ntfyPluginConfigJson;
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

  options.services.functora.vibeNtfy.enable = lib.mkEnableOption "ntfy.sh push notifications via the opencode-ntfy.sh plugin";

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
