{langs}: let
  pkgs = import ./nixpkgs.nix;
  full = import ./piper-voices-full.nix;
in
  pkgs.stdenv.mkDerivation {
    name = "piper-voices-${pkgs.lib.concatStringsSep "-" langs}";
    src = full;
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out
      cp "$src/voices.json" "$out/voices.json"
      for dir in ${pkgs.lib.concatStringsSep " " langs}; do
        cp -R "$src/$dir" "$out/$dir"
      done
    '';
  }
