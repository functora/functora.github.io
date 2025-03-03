let
  fj = import ./firejail.nix;
  misc = import ./misc.nix;
  pkgs = import ./nixpkgs.nix;
in [
  (
    fj.mkFirejailService {
      pkg = "tabby-server";
      dir = "tabby";
      cfg = ''
        # env SWC_DEBUG=1
        # env RUST_LOG=trace
        env TABBY_DISABLE_USAGE_COLLECTION=1
      '';
      exe = ''
        ${import ./tabby-socat.nix}/bin/tabby-socat \
          serve \
          --device vulkan \
          --model Qwen2.5-Coder-0.5B
      '';
    }
  )
  (
    misc.mkService {
      srv = "tabby-socket";
      mkExe = config: ''
        ${pkgs.socat}/bin/socat \
          TCP-LISTEN:8080,fork,reuseaddr,keepalive \
          UNIX:/home/${
          config.services.functora.userName
        }/.firejail/tabby/tabby.sock,keepalive
      '';
    }
  )
]
