let
  pkgs = import ./nixpkgs.nix;
in
  import (import ./sources.nix {}).nixpkgs {
    config.android_sdk.accept_license = true;
    config.allowUnfreePredicate = pkg:
      builtins.elem (pkgs.lib.getName pkg) [
        "android-sdk-tools"
        "android-sdk-cmdline-tools"
      ];
  }
