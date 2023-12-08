let
  pkgs = import ./../../../nix/nixpkgs.nix;
in
  {
    mini ? true,
    vimBackground ? "dark",
    vimColorScheme ? "PaperColor",
  }:
    with pkgs; let
      pkg = import ./. {
        inherit mini vimBackground vimColorScheme;
      };
    in
      stdenv.mkDerivation {
        name = "vi-shell";
        buildInputs =
          if mini
          then [pkg]
          else pkg;
        TERM = "xterm-256color";
      }
