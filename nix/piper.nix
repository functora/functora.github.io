let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "piper";
    text = ''
      export PIPER_VOICES="${import ./piper-voices-full.nix}"
      cmd="$*"
      eval "set -- $cmd"
      echo "==> show"
      echo "$@"
      echo "==> eval"
      ${pkgs.piper-tts}/bin/piper "$@"
    '';
  }
