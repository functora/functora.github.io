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
    url = "https://github.com/pa1nki113r/Project_Brutality/archive/1b5fdfb8cfcb58929833e2bccafa3ff05212e617.tar.gz";
    sha256 = "1d8r8sj5qlbg7i3j58r0n5jksdql70qk7lcaqq9cj443rz44bf7f";
  };
  bd = ../bak/doom/brutal22test6.pk3;
  qcde27 = "${../bak/doom/qcde27/QCDEv2.7c.pk3} ${../bak/doom/qcde27/QCDEmus2.5.pk3} ${../bak/doom/qcde27/QCDEmaps2.7.pk3} ${../bak/doom/qcde27/QCDE--Voxels2.2.pk3} ${../bak/doom/qcde27/QCDE--HDFaces2.7.pk3} ${../bak/doom/qcde27/GeorgeExleyAnnouncer.pk3} ${../bak/doom/qcde27/AeonQCDE.pk3} ${../bak/doom/CodeFX_v2.55.pk3}";
  qcde30 = "${../bak/doom/qcde30/QCDEv3.0.pk3} ${../bak/doom/qcde30/QCDEmus3.0.pk3} ${../bak/doom/qcde30/QCDEmaps3.0.pk3}";
  qcde31 = "${../bak/doom/qcde31/QCDEv3.1_beta_2.pk3} ${../bak/doom/qcde31/QCDEmus3.0.pk3} ${../bak/doom/qcde31/QCDEmaps3.0.pk3} ${../bak/doom/qcde31/QCDE_UT_Movement_v3.1_beta_2.pk3}";
  utde31 = "${qcde31} ${../bak/doom/qcde31/QCDE_UT_Weapons_v3.1_beta_1.pk3} ${../bak/doom/qcde31/QCDEmus_UT_v3.1_beta_1.pk3}";
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
    asound = pkgs.writeText "asound.conf" ''
      pcm.!default {
        type pulse
      }
      ctl.!default {
        type pulse
      }
    '';
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
          env.ALSA_PLUGIN_DIR = "${pkgs.alsa-plugins}/lib/alsa-lib";
          bind.ro = [
            [(toString passwd) "/etc/passwd"]
            [(toString asound) "/etc/asound.conf"]
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
    pkg ? "${pkgs.uzdoom}/bin/uzdoom",
    cfg ? null,
    wad ? ../bak/doom/wads/doom2.wad,
    sky ? ../bak/doom/CryosUltDoomSkies.wad,
    dec ? ../bak/doom/DestDec_v2.pk3,
    mod ? "",
    tex ? "",
    gfx ? ../bak/doom/CodeFX_v2.55.pk3,
    total ? ../bak/doom/DiamondDragon.pk3,
    music ? music_doom,
    lights ? ''"${duhd}/1 lights2.wad"'',
    liquid ? ltp4,
    relite ? ../bak/doom/relite_0.7.3b.pk3,
    parallax ? ''"${duhd}/0 Parallax PBR.pk3"'',
    nashgore ? "",
    movement ? "",
    flashlight ? ../bak/doom/flashlight_plus_plus_v9_1.pk3,
    blur ? ../bak/doom/Cynic_Games_ChromaBlur_v1.2lts.pk3,
    lastweapon ? ../bak/doom/fast-swap.pk3,
    cblood ? ../bak/doom/cblood.pk3,
  }: {
    "doom-${tag}" = mkDoomSand {
      name = "doom-${tag}";
      text = ''
        ${pkg} \
        -iwad ${wad} \
        -file \
        ${sky} \
        ${dec} \
        ${mod} \
        ${tex} \
        ${gfx} \
        ${total} \
        ${music} \
        ${lights} \
        ${liquid} \
        ${relite} \
        ${parallax} \
        ${nashgore} \
        ${movement} \
        ${flashlight} \
        ${../bak/doom/Cynic_Games_LensFlare_v_1.2.1.pk3} \
        ${blur} \
        ${lastweapon} \
        ${cblood} \
        ${
          if cfg == null
          then ""
          else "-exec ${cfg}"
        } "$@"
      '';
    };
  };
  mkDoom64 = args:
    mkDoom
    (
      {
        sky = ../bak/doom/Cran_D64Patch_skygenerator.pk3;
        tex = "${../bak/doom/Cran_D64PatchTex_v1.3.1.pk3} ${../bak/doom/Cran_D64Patch_BMapsD2_v2.pk3}";
        total = "${../bak/doom/BD64-VoH_game_v1.6.1.pk3} ${../bak/doom/BD64-VoH_maps_v1.6.1.pk3}";
        liquid = ltp701;
        movement = ../bak/doom/BD64_ZMovement.pk3;
      }
      // args
    );
  mkDoomPB = args:
    mkDoom (
      {
        total = pb;
        liquid = ltp701;
        blur = "";
      }
      // args
    );
  mkDoomBD = args:
    mkDoom (
      {
        total = bd;
        liquid = ltp701;
      }
      // args
    );
  games =
    pkgs.lib.optionalAttrs (builtins.pathExists ../bak/doom)
    (
      mkDoom {
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
      // mkDoom64 {
        tag = "plutonia";
        wad = ../bak/doom/wads/plutonia.wad;
      }
      // mkDoom64 {
        tag = "64";
        music = ../bak/doom/BD64-VoH_D64D2_Ost.pk3;
      }
      // mkDoomPB {
        tag = "annie";
        mod = ../bak/doom/Annie-E1-v1.1.zip;
        relite = ../bak/doom/relite_0.6.7a.pk3;
      }
      // mkDoomPB {
        tag = "phlegethon";
        mod = ../bak/doom/Phlegethon.wad;
        relite = ../bak/doom/relite_0.5a.pk3;
      }
      // mkDoomPB {
        tag = "dbp37-augzen";
        dec = "";
        mod = ../bak/doom/DBP37_AUGZEN.wad;
        relite = ../bak/doom/relite_0.5a.pk3;
      }
      // mkDoomPB {
        tag = "mmdcxiv-debut";
        mod = ../bak/doom/MMDCXIV-Debut.pk3;
        relite = "";
      }
      // mkDoomPB {
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
      // mkDoomPB {
        tag = "dark";
        mod = ../bak/doom/DUpart1.wad;
        relite = ../bak/doom/relite_0.6.7a.pk3;
      }
      // mkDoomPB {
        tag = "d2iro";
        mod = ../bak/doom/D2IRO.wad;
        relite = ../bak/doom/relite_0.5a.pk3;
      }
      // mkDoomPB {
        tag = "d2ico";
        mod = ../bak/doom/D2ICO.wad;
        relite = ../bak/doom/relite_0.5a.pk3;
      }
      // mkDoom {
        tag = "qcde";
        pkg = "${qz}/bin/q-zandronum";
        cfg = ../cfg/doom-qcde.cfg;
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
        tag = "utde";
        pkg = "${qz}/bin/q-zandronum";
        cfg = ../cfg/doom-utde.cfg;
        gfx = "";
        total = utde31;
        lights = "";
        relite = "";
        nashgore = "";
        parallax = "";
        flashlight = ../bak/doom/zand-flashlight.pk3;
        lastweapon = "";
      }
      // mkDoomPB {
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
        tag = "ashes-2";
        gfx = "";
        total = "${../bak/doom/ashes151/Resources/AshesSAMenu.pk3} ${../bak/doom/ashes151/Resources/lightmodepatch.pk3} ${../bak/doom/ashes151/Resources/AshesAfterglow1_16.pk3} ${../bak/doom/ashes-mods/Sprite-Fixes.pk3} ${../bak/doom/ashes-mods/ashesafterglow_buildmovev54.pk3} ${../bak/doom/ashes-mods/Ashes_AG_cutscene_update.pk3} ${../bak/doom/ashes-mods/ashes_nightvisor.pk3} ${../bak/doom/ashes-mods/AshesVoxelPickups-Ep2.pk3} ${../bak/doom/ashes-mods/ashes-wpn-tracers.pk3} ${../bak/doom/ashes-mods/Dan_Ashes_Afterglow_After_Effects.pk3}";
        liquid = "";
        lights = "";
        relite = "";
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "ashes-3";
        gfx = "";
        total = "${../bak/doom/ashes151/Resources/AshesSAMenu.pk3} ${../bak/doom/ashes151/Resources/lightmodepatch.pk3} ${../bak/doom/ashes151/Resources/AshesHardReset_105.pk3} ${../bak/doom/ashes-mods/ashes_flashlight_hitcharge-v2.pk3} ${../bak/doom/ashes-mods/asheshardreset_buildmovev54.pk3} ${../bak/doom/ashes-mods/Ashes_HR_ATHM_mod.pk3} ${../bak/doom/ashes-mods/Ashes_HR_cutscene_update.pk3} ${../bak/doom/ashes-mods/ashes_nightvisor.pk3} ${../bak/doom/ashes-mods/ashes-wpn-tracers.pk3} ${../bak/doom/ashes-mods/Dan_Ashes_Hard_Reset_After_Effects.pk3} ${../bak/doom/ashes-mods/Sprite-Fixes.pk3}";
        liquid = "";
        lights = "";
        relite = "";
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "ashes-4";
        gfx = "";
        total = "${../bak/doom/ashes151/Resources/AshesSAMenu.pk3} ${../bak/doom/ashes151/Resources/lightmodepatch.pk3} ${../bak/doom/ashes-mods/ashes-blackwater.pk3} ${../bak/doom/ashes-mods/Sprite-Fixes.pk3} ${../bak/doom/ashes-mods/ashesafterglow_buildmovev54.pk3} ${../bak/doom/ashes-mods/Ashes_AG_cutscene_update.pk3} ${../bak/doom/ashes-mods/AshesVoxelPickups-Ep2.pk3} ${../bak/doom/ashes-mods/ashes-wpn-tracers.pk3}";
        liquid = "";
        lights = "";
        relite = "";
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "ashes-5";
        gfx = "";
        total = "${../bak/doom/ashes151/Resources/AshesSAMenu.pk3} ${../bak/doom/ashes151/Resources/lightmodepatch.pk3} ${../bak/doom/ashes151/Resources/AshesAfterglow1_16.pk3} ${../bak/doom/ashes-mods/AshesNomad.pk3} ${../bak/doom/ashes-mods/Sprite-Fixes.pk3} ${../bak/doom/ashes-mods/ashesafterglow_buildmovev54.pk3} ${../bak/doom/ashes-mods/Ashes_AG_cutscene_update.pk3} ${../bak/doom/ashes-mods/ashes_nightvisor.pk3} ${../bak/doom/ashes-mods/AshesVoxelPickups-Ep2.pk3} ${../bak/doom/ashes-mods/ashes-wpn-tracers.pk3} ${../bak/doom/ashes-mods/Dan_Ashes_Afterglow_After_Effects.pk3}";
        liquid = "";
        lights = "";
        relite = "";
        nashgore = ../bak/doom/nashgore.pk3;
      }
      // mkDoom {
        tag = "venturos";
        mod = ''"${duhd}/10 HD_SFX.wad" "${duhd}/13 Tilt++.pk3"'';
        total = ../bak/doom/Venturous_1.7.0b.pk3;
        liquid = ltp701;
        relite = "";
        nashgore = ../bak/doom/nashgore.pk3;
        movement = ../bak/doom/ZMovement_3.2.1.pk3;
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
