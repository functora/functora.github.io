let
  nixpkgs = import ./nixpkgs22.nix;
in
  {
    pkgs ? import nixpkgs {},
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
