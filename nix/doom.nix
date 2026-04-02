{
  pkgs ? import <nixpkgs> {},
  user ? "doom",
}: let
  qz = pkgs.callPackage ./q-zandronum.nix {};
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
  qcde27 = "${../bak/doom/qcde27/QCDEv2.7c.pk3} ${../bak/doom/qcde27/QCDEmus2.5.pk3} ${../bak/doom/qcde27/QCDEmaps2.7.pk3} ${../bak/doom/qcde27/QCDE--Voxels2.2.pk3} ${../bak/doom/qcde27/QCDE--HDFaces2.7.pk3} ${../bak/doom/qcde27/GeorgeExleyAnnouncer.pk3} ${../bak/doom/qcde27/AeonQCDE.pk3} ${../bak/doom/CodeFX_v2.55.pk3}";
  qcde30 = "${../bak/doom/qcde30/QCDEv3.0.pk3} ${../bak/doom/qcde30/QCDEmus3.0.pk3} ${../bak/doom/qcde30/QCDEmaps3.0.pk3}";
  qcde31 = "${../bak/doom/qcde31/QCDEv3.1_beta_2.pk3} ${../bak/doom/qcde31/QCDEmus3.0.pk3} ${../bak/doom/qcde31/QCDEmaps3.0.pk3} ${../bak/doom/qcde31/QCDE_UT_Movement_v3.1_beta_2.pk3}";
  ltp701 = ''"${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP V7.0.1.pk3" "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Reflection Add-on (Must Add To Play)/LTP 16x9 Real Time Reflections Add-on/LTP 16x9 RT Reflection 2560x1440.pk3" "${../bak/doom/ltp701}/Liquid Texture Pack V7.0.1/LTP Demo Map + Map Editing + Add-on Files/LTP Add-on Files/LTP - Doom Terrain Splashes.pk3"'';
  ltp4 = ''"${../bak/doom/ltp4}/Liquid Texture Pack/(Zandronum) Liquid Texture pack V4.0.pk3" "${../bak/doom/ltp4}/Glowing Toxic Texture Pack/LTP V4.0 Glowing Toxic Texture Addon.pk3" "${../bak/doom/ltp4}/Shader Pack/LTP V4.0 Shader pack.pk3" "${../bak/doom/ltp4}/Shader Pack/LTP V4.0 Sky shader addon.pk3"'';
  music_doom = "${../bak/doom/Doom2016_OST.pk3} ${../bak/doom/DOOMIIHellOnEarth_DOOMEternal_OST.pk3}";
  music_juke = ../bak/doom/FerretJukeBoxV1-0.pk3;
  mkCod = mod: ''"${../bak/doom/cod-full}/(001)_CodV_FileA_BrutalV22test4_FIX.pk3" "${../bak/doom/cod-full}/(001)Addon_gearbox-0.7.3.pk3" ${mod} "${../bak/doom/cod-full}/ZZD_CodV_FileB_Graphics.wad" "${../bak/doom/cod-full}/ZZD_CodV_FileC_MainData.wad"'';
  mkDoomSand = {
    name,
    text,
    network ? false,
  }: let
    app = pkgs.writeShellApplication {
      inherit name text;
    };
    passwd = pkgs.writeTextFile {
      name = "passwd";
      text = "${user}:x:1000:1000:${user}:/home/${user}:/bin/sh";
    };
    sandbox = mkNixPak {
      config = {sloth, ...}: {
        app.package = app;
        gpu.enable = true;
        fonts.enable = true;
        locale.enable = true;
        bubblewrap = {
          inherit network;
          dieWithParent = true;
          sockets.pulse = true;
          sockets.wayland = true;
          bind.ro = [
            [(toString passwd) "/etc/passwd"]
          ];
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
    pkg ? "${pkgs.gzdoom}/bin/gzdoom",
    cfg ? null,
    wad ? ../bak/doom/wads/doom2.wad,
    sky ? ../bak/doom/CryosUltDoomSkies.wad,
    mod ? "",
    gfx ? ../bak/doom/CodeFX_v2.55.pk3,
    total ? ../bak/doom/DiamondDragon.pk3,
    music ? "",
    lights ? ''"${duhd}/1 lights2.wad"'',
    liquid ? ltp4,
    relite ? ../bak/doom/relite_0.7.3b.pk3,
    parallax ? ''"${duhd}/0 Parallax PBR.pk3"'',
    nashgore ? "",
    flashlight ? "${duhd}/12 Flashlight++.pk3",
    lastweapon ? ../bak/doom/fast-swap.pk3,
  }: {
    "doom-${tag}" = mkDoomSand {
      name = "doom-${tag}";
      text = ''
        ${pkg} \
        -iwad ${wad} \
        -file ${sky} ${../bak/doom/DestDec_v2.pk3} \
        ${mod} \
        ${gfx} \
        ${total} \
        ${music} \
        ${lights} \
        ${liquid} \
        ${relite} \
        ${parallax} \
        ${nashgore} \
        ${flashlight} \
        ${../bak/doom/Cynic_Games_LensFlare_v_1.2.1.pk3} \
        ${../bak/doom/Cynic_Games_ChromaBlur_v1.2lts.pk3} \
        ${../bak/doom/cblood.pk3} \
        ${lastweapon} \
        ${
          if cfg == null
          then ""
          else "-exec ${cfg}"
        } "$@"
      '';
    };
  };
  games =
    pkgs.lib.optionalAttrs (builtins.pathExists ../bak/doom)
    (
      {
        doom-seeker = mkDoomSand {
          name = "doom-seeker";
          text = ''${pkgs.doomseeker}/bin/doomseeker "$@"'';
          network = true;
        };
      }
      // mkDoom {
        tag = "free1";
        wad = "${free}/freedoom1.wad";
        relite = ../bak/doom/relite_0.6.7a.pk3;
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
        tag = "tnt2";
        mod = ../bak/doom/TNT2_1_1.wad;
        gfx = ../bak/doom/CodeFX_v1.101.pk3;
        relite = "";
      }
      // mkDoom {
        tag = "plutonia";
        wad = ../bak/doom/wads/plutonia.wad;
      }
      // mkDoom {
        tag = "annie1";
        mod = ../bak/doom/Annie-E1-v1.1.zip;
        relite = ../bak/doom/relite_0.6.7a.pk3;
      }
      // mkDoom {
        tag = "scorched-heritage";
        mod = ../bak/doom/ScorchedHeritage.wad;
      }
      // mkDoom {
        tag = "slum-alley";
        mod = ../bak/doom/SlumAlley.wad;
      }
      // mkDoom {
        tag = "i-hate-storm";
        mod = ../bak/doom/IHateStorm.wad;
        relite = "";
      }
      // mkDoom {
        tag = "lilliput-lane";
        mod = ../bak/doom/LilliputLane.wad;
      }
      // mkDoom {
        tag = "rmg-adcd";
        mod = ../bak/doom/RMG_ADCD.wad;
      }
      // mkDoom {
        tag = "rmg-urban";
        mod = ../bak/doom/RMG-UrbnCrsd_24-04-23.wad;
      }
      // mkDoom {
        tag = "rmg-city";
        mod = ../bak/doom/RMG_City.wad;
        relite = "";
      }
      // mkDoom {
        tag = "viaje";
        mod = ../bak/doom/VIAJE.wad;
      }
      // mkDoom {
        tag = "hellbnd";
        mod = ../bak/doom/Hellbnd.wad;
      }
      // mkDoom {
        tag = "c17";
        total = ../bak/doom/DTB_C17.pk3;
        relite = "";
      }
      // mkDoom {
        tag = "hell-fire";
        mod = ../bak/doom/HellFireCollectionV1-6.pk3;
      }
      // mkDoom {
        tag = "dbp37-augzen";
        mod = ../bak/doom/DBP37_AUGZEN.wad;
        relite = "";
      }
      // mkDoom {
        tag = "mmdcxiv-debut";
        mod = ../bak/doom/MMDCXIV-Debut.pk3;
        relite = "";
      }
      // mkDoom {
        tag = "dex";
        mod = ../bak/doom/DEX_1.wad;
      }
      // mkDoom {
        tag = "nostalgic-entropy";
        mod = ../bak/doom/NE.wad;
        relite = "";
      }
      // mkDoom {
        tag = "neon-overdrive";
        mod = ../bak/doom/NEONOVER.wad;
        relite = "";
      }
      // mkDoom {
        tag = "city-assault";
        mod = ../bak/doom/city-assault.wad;
      }
      // mkDoom {
        tag = "slime";
        mod = ../bak/doom/SLIMECTY.wad;
        relite = "";
      }
      // mkDoom {
        tag = "ihni";
        total = ../bak/doom/ihni-1.04.pk3;
        relite = "";
      }
      // mkDoom {
        tag = "lostdamn";
        mod = ../bak/doom/lostdamn.pk3;
        relite = "";
      }
      // mkDoom {
        tag = "qcde";
        pkg = "${qz}/bin/q-zandronum";
        cfg = ../cfg/doom.cfg;
        gfx = "";
        total = qcde31;
        lights = "";
        relite = "";
        nashgore = "";
        parallax = "";
        flashlight = ../bak/doom/zand-flashlight.pk3;
        lastweapon = "";
      }
      // mkDoom {
        wad = ../bak/doom/wads/doom.wad;
        tag = "spectacle-creep";
        mod = ../bak/doom/spectacle_creep_build10D.wad;
        relite = "";
      }
      // mkDoom {
        tag = "cats";
        mod = ''"${duhd}/10 HD_SFX.wad" "${duhd}/13 Tilt++.pk3"'';
        total = ../bak/doom/Space_Cats_Saga_1.41.wad;
        relite = ../bak/doom/relite_0.5a.pk3;
        liquid = ltp701;
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "bloom";
        mod = ''"${duhd}/10 HD_SFX.wad" "${duhd}/13 Tilt++.pk3"'';
        total = ../bak/doom/bloom/Bloom.pk3;
        liquid = ltp701;
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "trench";
        total = "${../bak/doom/TF-maps.pk3} ${../bak/doom/TrenchFoot.pk3}";
        relite = "";
        liquid = ltp701;
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "butcher";
        total = ../bak/doom/FN-TrenchFoot.pk3;
        relite = ../bak/doom/relite_0.5a.pk3;
        liquid = ltp701;
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "infinite";
        mod = ''"${duhd}/10 HD_SFX.wad" "${duhd}/13 Tilt++.pk3"'';
        total = ../bak/doom/DOOM_Infinite_098_PP2_H2.pk3;
        music = music_doom;
        liquid = ''"${duhd}/26 Liquids.pk3"'';
        relite = "";
        nashgore = ../bak/doom/nashgore.pk3;
      }
    );
in
  pkgs.symlinkJoin {
    name = "doom-games";
    paths = pkgs.lib.attrValues games;
  }
