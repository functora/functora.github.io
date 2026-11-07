{
  pkgs ? import <nixpkgs> {},
  user ? "quake",
}: let
  mkBubble = import ./bubble.nix;
  ironwail = pkgs.ironwail;
  zircon = pkgs.callPackage ./zircon.nix {};
  unzip = pkgs.unzip;
  q1Cfg = ../cfg/q1.cfg;

  # Base Quake game data options: pak0.pak + pak1.pak (deterministic, pinned)
  quakeSrc = pkgs.fetchzip {
    url = "https://archive.org/download/quake_pak_202306/quake_pak.zip";
    hash = "sha256-fBfERuldsDCqy3Bfgby64ZYs9B4iQ07xVNvVIFldCps=";
    stripRoot = false;
  };
  # id1 directory holding the base game paks (default base)
  quakeDer = pkgs.stdenv.mkDerivation {
    name = "quake-id1";
    src = null;
    dontUnpack = true;
    buildInputs = [pkgs.coreutils];
    installPhase = ''
      mkdir -p $out
      cp ${quakeSrc}/pak0.pak $out/
      cp ${quakeSrc}/pak1.pak $out/
      cp ${q1Cfg} $out/q1.cfg
    '';
  };
  # Alternative base: libre quake (id1/ subdirectory)
  librequakeSrc = pkgs.fetchzip {
    url = "https://github.com/lavenderdotpet/LibreQuake/releases/download/v0.09-beta/full.zip";
    hash = "sha256-i2FAFiZZejcg09zBLfDFZRAHNgNVUn1cSoIZ3+4AmP0=";
  };

  qbj3Src = pkgs.fetchzip {
    url = "https://us.22061996.xyz/qbj3/qbj3_1.3.zip";
    hash = "sha256-Gp3vOnTWsLS1wS/0L+MhAgfFlZR9d/ZS1doOlNvEe/0=";
  };
  qbj3Cfg = pkgs.stdenv.mkDerivation {
    name = "qbj3-with-q1cfg";
    src = qbj3Src;
    dontUnpack = true;
    buildInputs = [pkgs.coreutils];
    installPhase = ''
      mkdir -p $out
      cp -r $src/* $out/
      cp ${q1Cfg} $out/q1.cfg
    '';
  };
  adSrc = pkgs.fetchzip {
    url = "https://www.quaddicted.com/filebase/ad_v1_80p1final.zip";
    hash = "sha256-4Iq1Huh2qUDzXv2prx9yF/w548vta2XhBKFiqYXoL8s=";
    stripRoot = false;
  };
  adCfg = pkgs.stdenv.mkDerivation {
    name = "ad-with-q1cfg";
    src = adSrc;
    dontUnpack = true;
    buildInputs = [pkgs.coreutils];
    installPhase = ''
      mkdir -p $out
      cp -r $src/* $out/
      cp ${q1Cfg} $out/q1.cfg
    '';
  };
  combatSrc = pkgs.fetchurl {
    url = "https://archive.org/download/combat-v-5.7.9/Combat_v5.7.9.zip";
    hash = "sha256-ai7yg57Djz+AU1Vbl/Yx6M23TCQqH4owz9yYhKGUgS4=";
  };
  combatDer = pkgs.stdenv.mkDerivation {
    name = "combat-mod";
    src = combatSrc;
    dontUnpack = true;
    buildInputs = [unzip];
    installPhase = ''
      mkdir -p $out
      cd $out
      unzip $src
      cp ${q1Cfg} quake15/q1.cfg
    '';
  };

  mkQuakeSand = {
    name,
    text,
    base ? quakeDer,
    mods ? [],
  }: let
    root = pkgs.linkFarm "${name}-root" (
      mods
      ++ [
        {
          name = "id1";
          path = base;
        }
      ]
    );
  in
    mkBubble {
      inherit pkgs name text user;
      home = "quake";
      mkOverlay = _: [
        {
          path = ["bubblewrap" "bind" "ro"];
          update = prev: prev ++ [[(toString root) "/quake"]];
        }
      ];
    };
  mkQuake = {
    tag,
    engine ? ironwail,
    base ? quakeDer,
    mods ? [],
    game ? null,
    args ? "",
  }: {
    "quake-${tag}" = mkQuakeSand {
      name = "quake-${tag}";
      inherit base mods;
      text = ''
        cd /quake
        ${engine}/bin/${engine.pname} -basedir /quake ${
          if game == null
          then ""
          else "-game ${game}"
        } ${args} +exec q1.cfg "$@"
      '';
    };
  };
  games =
    mkQuake {
      tag = "base";
    }
    // mkQuake {
      tag = "qbj3";
      game = "qbj3";
      mods = [
        {
          name = "qbj3";
          path = qbj3Cfg;
        }
      ];
    }
    // mkQuake {
      tag = "arcane-dimensions";
      game = "ad";
      mods = [
        {
          name = "ad";
          path = adCfg;
        }
      ];
    }
    // mkQuake {
      tag = "combat";
      engine = zircon;
      mods = [
        {
          name = "quake15";
          path = "${combatDer}/quake15";
        }
      ];
      game = "quake15";
    };
in
  pkgs.symlinkJoin {
    name = "quake-games";
    paths = pkgs.lib.attrValues games;
  }
