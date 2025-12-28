{pkgs ? import <nixpkgs> {}}: let
  duhd = ../bak/doom/duhd;
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  mkDoom = {
    name,
    text,
  }: let
    app = pkgs.writeShellApplication {
      inherit name text;
    };
    sandbox = mkNixPak {
      config = {sloth, ...}: {
        app.package = app;
        gpu.enable = true;
        fonts.enable = true;
        locale.enable = true;
        bubblewrap = {
          network = false;
          sockets.pulse = true;
          sockets.wayland = true;
          bind.rw = [
            [
              (sloth.mkdir (sloth.concat' sloth.homeDir "/doom"))
              sloth.homeDir
            ]
          ];
          tmpfs = [
            "/tmp"
          ];
        };
      };
    };
  in
    sandbox.config.env;
in
  pkgs.lib.optionalAttrs (builtins.pathExists ../bak/doom)
  {
    infinite = mkDoom {
      name = "doom-infinite";
      text = ''
        ${pkgs.gzdoom}/bin/gzdoom \
          -iwad ${../bak/doom/wads/doom2.wad} \
          -file ${../bak/doom/DOOM_Infinite_098_PP2_H2.pk3} \
          -file "${duhd}/1 lights2.wad" "${duhd}/8 DHTP Textures.pk3" "${duhd}/9 JFO.wad" "${duhd}/10 HD_SFX.wad" "${duhd}/12 Flashlight++.pk3" "${duhd}/13 Tilt++.pk3" "${duhd}/14 brightmaps2.wad" "${duhd}/16 d3snds.wad" "${duhd}/19 SpriteShadow.wad" "${duhd}/20 WorldGamma.wad" "${duhd}/21 BloomBoost.wad" "${duhd}/22 MotionBlur.pk3" "${duhd}/23 hires_decals.wad" "${duhd}/24 Terrains.wad" "${duhd}/25 HD HUD.pk3" "${duhd}/26 Liquids.pk3" "${duhd}/27 marcelus_hd_sprites.pk3" "${duhd}/29 Universal Rain and Snow v3.pk3" "${duhd}/31 texture_lights.wad" "${duhd}/0 Parallax PBR.pk3" \
          -file ${../bak/doom/HD_Map_Enhancements.wad} \
          -file ${../bak/doom/nashgore.pk3} \
          -file ${../bak/doom/MaterialsNashgoreNext.pk3} \
          -file ${../bak/doom/Doom2016_OST.pk3} \
          -file ${../bak/doom/DOOMIIHellOnEarth_DOOMEternal_OST.pk3} \
          -file ${../bak/doom/cblood.pk3} \
          -file ${../bak/doom/SimpleSlots.1.1.pk7}
      '';
    };
  }
