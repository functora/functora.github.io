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
          --model DeepSeekCoder-1.3B
      '';
    }
  )
]
