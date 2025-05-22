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
    // fj.mkFirejailCustom rec {
      pkg = "doom-ashes1";
      dir = "doom";
      exe = ''
        ${pkgs.gzdoom}/bin/gzdoom \
          -iwad ${../bak/doom/freedoom-0.13.0/freedoom2.wad} \
          -file ${../bak/doom/AshesStandalone_V1_51/Resources/AshesSAMenu.pk3} \
          -file ${../bak/doom/AshesStandalone_V1_51/Resources/lightmodepatch.pk3} \
          -file ${../bak/doom/AshesStandalone_V1_51/Resources/Ashes2063Enriched2_23.pk3} \
          -file ${../bak/doom/AshesStandalone_V1_51/Resources/Ashes2063EnrichedFDPatch.pk3} \
          -file ${../bak/doom/2063_Ash3sD.pk3} \
          -file ${../bak/doom/AshesMiniMods/AshesDMWstart.pk3} \
          -file ${../bak/doom/AshesSmooth/ashes2063-smooth-enemies.pk3} \
          -file ${../bak/doom/AshesVoxelPickups-Ep1.pk3} \
          -file ${../bak/doom/ashes-wpn-tracers.pk3} \
          -file ${../bak/doom/relite_0.7.3b.pk3} \
          -file ${../bak/doom/SimpleSlots.1.1.pk7}
      '';
    }
  )
