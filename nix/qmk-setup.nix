let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication rec {
    name = "qmk-setup";
    text = ''
      ${pkgs.qmk}/bin/qmk setup
      TARGET="$HOME/qmk_firmware/keyboards/planck/keymaps/functora"
      if [ -L "$TARGET" ]; then
        echo "${name} ==> keymap symlink already exist!"
      else
        echo "${name} ==> creating symlink $TARGET"
        ln -s ${toString ../qmk} "$TARGET"
        echo "${name} ==> created keymap symlink!"
      fi
      qmk config user.keyboard=planck/rev6_drop
      qmk config user.keymap=functora
    '';
  }
