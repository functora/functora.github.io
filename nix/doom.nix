let
  olds = import ./oldpkgs.nix;
  pkgs = import ./nixpkgs.nix;
  unst = import ./nixpkgs-unstable.nix;
  vkdm = import ./vkdoom.nix {inherit pkgs;};
  fj = import ./firejail.nix;
  duhd = ../bak/doom/duhd;
in
  pkgs.lib.optionalAttrs (builtins.pathExists ../bak/doom)
  (
    fj.mkFirejailCustom {
      pkg = "doom-64";
      dir = "doom";
      exe = ''
        ${unst.gzdoom}/bin/gzdoom \
          -iwad ${../bak/doom/freedoom-0.13.0/freedoom2.wad} \
          -file ${../bak/doom/BD64-VoH_game_v1.4.2.pk3} \
          -file ${../bak/doom/BD64-VoH_maps_v1.4.2.pk3} \
          -file ${../bak/doom/BD64-VoH_MoreSFX.pk3} \
          -file ${../bak/doom/relite_0.7.3b.pk3} \
          -file ${../bak/doom/SimpleSlots.1.1.pk7}
      '';
    }
    // fj.mkFirejailCustom {
      pkg = "doom-cats";
      dir = "doom";
      exe = ''
        ${unst.gzdoom}/bin/gzdoom \
          -iwad ${../bak/doom/freedoom-0.13.0/freedoom2.wad} \
          -file ${../bak/doom/DMSF_ALX-R3.WAD} \
          -file "${duhd}/9 JFO.wad" \
          -file "${duhd}/27 marcelus_hd_sprites.pk3" \
          -file ${../bak/doom/space-cats-saga-1-41.wad} \
          -file "${duhd}/1 lights2.wad" "${duhd}/8 DHTP Textures.pk3" "${duhd}/10 HD_SFX.wad" "${duhd}/12 Flashlight++.pk3" "${duhd}/13 Tilt++.pk3" "${duhd}/14 brightmaps2.wad" "${duhd}/16 d3snds.wad" "${duhd}/17 brutaldoom_stuff.wad" "${duhd}/19 SpriteShadow.wad" "${duhd}/20 WorldGamma.wad" "${duhd}/21 BloomBoost.wad" "${duhd}/22 MotionBlur.pk3" "${duhd}/23 hires_decals.wad" "${duhd}/24 Terrains.wad" "${duhd}/25 HD HUD.pk3" "${duhd}/26 Liquids.pk3" "${duhd}/29 Universal Rain and Snow v3.pk3" "${duhd}/30 OST Remake.pk3" "${duhd}/31 texture_lights.wad" "${duhd}/0 Parallax PBR.pk3" \
          -file ${../bak/doom/CodeFX255NoFatsoNoArchvile.pk3} \
          -file ${../bak/doom/CodeFXFireNoLostSoul.pk3} \
          -file ${../bak/doom/CodeFXBlood.pk3} \
          -file "${../bak/doom/liquid}/Liquid Texture Pack/(GZDoom) Liquid Texture Pack V4.0.pk3" \
          -file "${../bak/doom/liquid}/Glowing Toxic Texture Pack/LTP V4.0 Glowing Toxic Texture Addon.pk3" \
          -file "${../bak/doom/liquid}/Shader Pack/LTP V4.0 Shader pack.pk3" \
          -file "${../bak/doom/liquid}/Shader Pack/LTP V4.0 Sky shader addon.pk3" \
          -file ${../bak/doom/SimpleSlots.1.1.pk7} \
          -file ${../bak/doom/nashgore.pk3} \
          -file ${../bak/doom/cblood.pk3}
      '';
    }
    // fj.mkFirejailCustom {
      pkg = "doom-ashes1";
      dir = "doom";
      exe = ''
        ${unst.gzdoom}/bin/gzdoom \
          -iwad ${../bak/doom/freedoom-0.13.0/freedoom2.wad} \
          -file ${../bak/doom/DMSF_ALX-R3.WAD} \
          -file ${../bak/doom/HD_Map_Enhancements.wad} \
          -file "${duhd}/1 lights2.wad" "${duhd}/8 DHTP Textures.pk3" "${duhd}/9 JFO.wad" "${duhd}/10 HD_SFX.wad" "${duhd}/12 Flashlight++.pk3" "${duhd}/13 Tilt++.pk3" "${duhd}/14 brightmaps2.wad" "${duhd}/16 d3snds.wad" "${duhd}/17 brutaldoom_stuff.wad" "${duhd}/19 SpriteShadow.wad" "${duhd}/20 WorldGamma.wad" "${duhd}/21 BloomBoost.wad" "${duhd}/22 MotionBlur.pk3" "${duhd}/23 hires_decals.wad" "${duhd}/24 Terrains.wad" "${duhd}/25 HD HUD.pk3" "${duhd}/26 Liquids.pk3" "${duhd}/27 marcelus_hd_sprites.pk3" "${duhd}/29 Universal Rain and Snow v3.pk3" "${duhd}/30 OST Remake.pk3" "${duhd}/31 texture_lights.wad" "${duhd}/0 Parallax PBR.pk3" \
          -file ${../bak/doom/CodeFX.pk3} \
          -file ${../bak/doom/CodeFXFire.pk3} \
          -file ${../bak/doom/CodeFXBlood.pk3} \
          -file "${../bak/doom/liquid}/Liquid Texture Pack/(GZDoom) Liquid Texture Pack V4.0.pk3" \
          -file "${../bak/doom/liquid}/Glowing Toxic Texture Pack/LTP V4.0 Glowing Toxic Texture Addon.pk3" \
          -file "${../bak/doom/liquid}/Shader Pack/LTP V4.0 Shader pack.pk3" \
          -file "${../bak/doom/liquid}/Shader Pack/LTP V4.0 Sky shader addon.pk3" \
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
          -file ${../bak/doom/nashgore.pk3} \
          -file ${../bak/doom/cblood.pk3} \
          -file ${../bak/doom/SimpleSlots.1.1.pk7}
      '';
    }
    // fj.mkFirejailCustom rec {
      pkg = "doom-infinite";
      dir = pkg;
      exe = ''
        ${unst.gzdoom}/bin/gzdoom \
          -iwad ${../bak/doom/freedoom-0.13.0/freedoom2.wad} \
          -file ${../bak/doom/DOOM_Infinite_098_PP2_H2.pk3} \
          -file ${../bak/doom/DMSF_ALX-R3.WAD} \
          -file "${duhd}/1 lights2.wad" "${duhd}/8 DHTP Textures.pk3" "${duhd}/9 JFO.wad" "${duhd}/10 HD_SFX.wad" "${duhd}/12 Flashlight++.pk3" "${duhd}/13 Tilt++.pk3" "${duhd}/14 brightmaps2.wad" "${duhd}/16 d3snds.wad" "${duhd}/17 brutaldoom_stuff.wad" "${duhd}/19 SpriteShadow.wad" "${duhd}/20 WorldGamma.wad" "${duhd}/21 BloomBoost.wad" "${duhd}/22 MotionBlur.pk3" "${duhd}/23 hires_decals.wad" "${duhd}/24 Terrains.wad" "${duhd}/25 HD HUD.pk3" "${duhd}/26 Liquids.pk3" "${duhd}/27 marcelus_hd_sprites.pk3" "${duhd}/29 Universal Rain and Snow v3.pk3" "${duhd}/31 texture_lights.wad" "${duhd}/0 Parallax PBR.pk3" \
          -file ${../bak/doom/HD_Map_Enhancements.wad} \
          -file ${../bak/doom/DoomInfiniteWeaponsSkins.pk3} \
          -file "${../bak/doom/liquid}/Liquid Texture Pack/(GZDoom) Liquid Texture Pack V4.0.pk3" \
          -file "${../bak/doom/liquid}/Glowing Toxic Texture Pack/LTP V4.0 Glowing Toxic Texture Addon.pk3" \
          -file "${../bak/doom/liquid}/Shader Pack/LTP V4.0 Shader pack.pk3" \
          -file "${../bak/doom/liquid}/Shader Pack/LTP V4.0 Sky shader addon.pk3" \
          -file ${../bak/doom/UltimateDoom2016OST.pk3} \
          -file ${../bak/doom/DOOMIIHellOnEarth_DOOMEternal_OST.pk3} \
          -file ${../bak/doom/Barrels.pk3} \
          -file ${../bak/doom/nashgore.pk3} \
          -file ${../bak/doom/dblood.pk3} \
          -file ${../bak/doom/bulletzborn.pk3} \
          -file ${../bak/doom/puff.pk3} \
          -file ${../bak/doom/SimpleSlots.1.1.pk7}
      '';
    }
  )
