{pkgs ? import <nixpkgs> {}}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  duhd = ../bak/doom/duhd;
  free = pkgs.fetchzip {
    url = "https://github.com/freedoom/freedoom/releases/download/v0.13.0/freedoom-0.13.0.zip";
    sha256 = "sha256-ieYfr4TYVRGUVriK/duN+iOlr8oAIAxz4IfnbG4hOis=";
  };
  pb = fetchTarball {
    url = "https://github.com/pa1nki113r/Project_Brutality/archive/3f0b2f51d66ba6dc9f2d316570fa78c04a84101a.tar.gz";
    sha256 = "0x13mllpc6qrc5w7vwxmz9ijpdy9648z5vvlk4s67wsxlslbw771";
  };
  ltp701 = ''"${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP V7.0.1.pk3" "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Reflection Add-on (Must Add To Play)/LTP 16x9 Real Time Reflections Add-on/LTP 16x9 RT Reflection 2560x1440.pk3" "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Demo Map + Map Editing + Add-on Files/LTP Add-on Files/LTP - Doom Terrain Splashes.pk3"'';
  mkDoomSand = {
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
  mkDoom = {
    tag,
    wad ? ../bak/doom/wads/doom2.wad,
    mod ? "",
    gfx ? ../bak/doom/CodeFX_v2.55.pk3,
    total ? pb,
    music ? "${../bak/doom/Doom2016_OST.pk3} ${../bak/doom/DOOMIIHellOnEarth_DOOMEternal_OST.pk3}",
    liquid ? ltp701,
    relite ? ../bak/doom/relite_0.6.7a.pk3,
    nashgore ? "",
  }: {
    "doom-${tag}" = mkDoomSand {
      name = "doom-${tag}";
      text = ''
        ${pkgs.gzdoom}/bin/gzdoom \
          -iwad ${wad} \
          -file ${../bak/doom/CryosUltDoomSkies.wad} ${../bak/doom/DestDec_v2.pk3} \
          ${mod} \
          ${gfx} \
          ${total} \
          ${music} \
          "${duhd}/1 lights2.wad" "${duhd}/12 Flashlight++.pk3" "${duhd}/0 Parallax PBR.pk3" \
          ${liquid} \
          ${relite} \
          ${../bak/doom/Cynic_Games_LensFlare_v_1.pk3} \
          ${nashgore} \
          ${../bak/doom/cblood.pk3} \
          ${../bak/doom/LastWeapon.pk3}
      '';
    };
  };
  games =
    pkgs.lib.optionalAttrs (builtins.pathExists ../bak/doom)
    (mkDoom {
        tag = "free1";
        wad = "${free}/freedoom1.wad";
      }
      // mkDoom {
        tag = "free2";
        wad = "${free}/freedoom2.wad";
      }
      // mkDoom {
        tag = "1";
        wad = ../bak/doom/wads/doomu.wad;
      }
      // mkDoom {
        tag = "2";
      }
      // mkDoom {
        tag = "tnt";
        wad = ../bak/doom/wads/tnt.wad;
      }
      // mkDoom {
        tag = "plutonia";
        wad = ../bak/doom/wads/plutonia.wad;
      }
      // mkDoom {
        tag = "annie";
        mod = ../bak/doom/Annie-E1-v1.1.zip;
      }
      // mkDoom {
        tag = "scorched-heritage";
        mod = ../bak/doom/ScorchedHeritage.wad;
        relite = "";
      }
      // mkDoom {
        tag = "slum-alley";
        mod = ../bak/doom/SlumAlley.wad;
        relite = "";
      }
      // mkDoom {
        tag = "eviternity";
        mod = ../bak/doom/Eviternity.wad;
        relite = "";
      }
      // mkDoom {
        tag = "cats";
        mod = ''"${duhd}/10 HD_SFX.wad" "${duhd}/13 Tilt++.pk3" "${duhd}/22 MotionBlur.pk3"'';
        total = ../bak/doom/Space_Cats_Saga_1.41.wad;
        music = "";
        relite = ../bak/doom/relite_0.5a.pk3;
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "bloom";
        mod = ''"${duhd}/10 HD_SFX.wad" "${duhd}/13 Tilt++.pk3" "${duhd}/22 MotionBlur.pk3"'';
        total = ../bak/doom/bloom/Bloom.pk3;
        music = "";
        relite = ../bak/doom/relite_0.7.3b.pk3;
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "infinite";
        mod = ''"${duhd}/10 HD_SFX.wad" "${duhd}/13 Tilt++.pk3" "${duhd}/22 MotionBlur.pk3"'';
        total = ../bak/doom/DOOM_Infinite_098_PP2_H2.pk3;
        liquid = ''"${duhd}/26 Liquids.pk3"'';
        relite = "";
        nashgore = ../bak/doom/nashgore.pk3;
      });
in
  pkgs.symlinkJoin {
    name = "doom-games";
    paths = pkgs.lib.attrValues games;
  }
