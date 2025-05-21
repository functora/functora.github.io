let
  pkgs = import ./nixpkgs.nix;
  fj = import ./firejail.nix;
in
  pkgs.lib.optionalAttrs (builtins.pathExists ../bak/doom)
  (
    fj.mkFirejailCustom rec {
      pkg = "doom-64";
      dir = "doom";
      exe = ''
        ${pkgs.gzdoom}/bin/gzdoom \
          -iwad ${../bak/doom/freedoom-0.13.0/freedoom2.wad} \
          -file ${../bak/doom/BD64-VoH_game_v1.4.2.pk3} \
          -file ${../bak/doom/BD64-VoH_maps_v1.4.2.pk3} \
          -file ${../bak/doom/BD64-VoH_MoreSFX.pk3} \
          -file ${../bak/doom/relite_0.7.3b.pk3} \
          -file ${../bak/doom/SimpleSlots.1.1.pk7}
      '';
    }
  )
