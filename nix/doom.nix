{pkgs ? import <nixpkgs> {}}: let
  pb = fetchTarball {
    url = "https://github.com/pa1nki113r/Project_Brutality/archive/3f0b2f51d66ba6dc9f2d316570fa78c04a84101a.tar.gz";
    sha256 = "0x13mllpc6qrc5w7vwxmz9ijpdy9648z5vvlk4s67wsxlslbw771";
  };
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
  mkDoomPb = {
    tag,
    wad,
  }: let
    name = "doom-pb-${tag}";
  in {
    "${name}" = mkDoom {
      inherit name;
      text = ''
        ${pkgs.gzdoom}/bin/gzdoom \
          -iwad ${wad} \
          -file ${../bak/doom/HD_Map_Enhancements.wad} \
          -file ${pb} \
          -file "${duhd}/1 lights2.wad" "${duhd}/14 brightmaps2.wad" "${duhd}/20 WorldGamma.wad" "${duhd}/21 BloomBoost.wad" "${duhd}/0 Parallax PBR.pk3" \
          -file "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP V7.0.1.pk3" \
          -file "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Reflection Add-on (Must Add To Play)/LTP 16x9 Real Time Reflections Add-on/LTP 16x9 RT Reflection 1920x1080 .pk3" \
          -file "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Demo Map + Map Editing + Add-on Files/LTP Add-on Files/LTP - Doom Terrain Splashes.pk3" \
          -file ${../bak/doom/relite_0.7.3b.pk3} \
          -file ${../bak/doom/Doom2016_OST.pk3} \
          -file ${../bak/doom/DOOMIIHellOnEarth_DOOMEternal_OST.pk3} \
          -file ${../bak/doom/cblood.pk3} \
          -file ${../bak/doom/SimpleSlots.1.1.pk7}
      '';
    };
  };
  games =
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
            -file ${../bak/doom/Doom2016_OST.pk3} \
            -file ${../bak/doom/DOOMIIHellOnEarth_DOOMEternal_OST.pk3} \
            -file ${../bak/doom/cblood.pk3} \
            -file ${../bak/doom/SimpleSlots.1.1.pk7}
        '';
      };
      bloom = mkDoom {
        name = "doom-bloom";
        text = ''
          ${pkgs.gzdoom}/bin/gzdoom \
            -iwad ${../bak/doom/wads/doom2.wad} \
            -file ${../bak/doom/bloom/gzdoom-4-6-0-Windows-64bit/Bloom.pk3} \
            -file ${../bak/doom/bloom/gzdoom-4-6-0-Windows-64bit/game_support.pk3} \
            -file ${../bak/doom/bloom/gzdoom-4-6-0-Windows-64bit/game_widescreen_gfx.pk3} \
            -file "${duhd}/1 lights2.wad" "${duhd}/12 Flashlight++.pk3" "${duhd}/13 Tilt++.pk3" "${duhd}/14 brightmaps2.wad" "${duhd}/20 WorldGamma.wad" "${duhd}/21 BloomBoost.wad" "${duhd}/0 Parallax PBR.pk3" \
            -file "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP V7.0.1.pk3" \
            -file "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Reflection Add-on (Must Add To Play)/LTP 16x9 Real Time Reflections Add-on/LTP 16x9 RT Reflection 1920x1080 .pk3" \
            -file "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Demo Map + Map Editing + Add-on Files/LTP Add-on Files/LTP - Doom Terrain Splashes.pk3" \
            -file ${../bak/doom/nashgore.pk3} \
            -file ${../bak/doom/cblood.pk3} \
            -file ${../bak/doom/relite_0.7.3b.pk3} \
            -file ${../bak/doom/SimpleSlots.1.1.pk7}
        '';
      };
    }
    // mkDoomPb {
      tag = "1";
      wad = ../bak/doom/wads/doomu.wad;
    }
    // mkDoomPb {
      tag = "2";
      wad = ../bak/doom/wads/doom2.wad;
    }
    // mkDoomPb {
      tag = "tnt";
      wad = ../bak/doom/wads/tnt.wad;
    }
    // mkDoomPb {
      tag = "plutonia";
      wad = ../bak/doom/wads/plutonia.wad;
    };
in
  pkgs.symlinkJoin {
    name = "doom-games";
    paths = pkgs.lib.attrValues games;
  }
