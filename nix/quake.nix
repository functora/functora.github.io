{
  pkgs ? import <nixpkgs> {},
  user ? "quake",
}: let
  mkBubble = import ./bubble.nix;
  darkplaces = pkgs.darkplaces;
  ironwail = pkgs.ironwail;
  p7zip = pkgs.p7zip;
  librequake = pkgs.fetchzip {
    url = "https://github.com/lavenderdotpet/LibreQuake/releases/download/v0.09-beta/full.zip";
    hash = "sha256-i2FAFiZZejcg09zBLfDFZRAHNgNVUn1cSoIZ3+4AmP0=";
  };
  qbj3Mod = pkgs.fetchzip {
    url = "https://us.22061996.xyz/qbj3/qbj3_1.3.zip";
    hash = "sha256-Gp3vOnTWsLS1wS/0L+MhAgfFlZR9d/ZS1doOlNvEe/0=";
  };
  adMod = pkgs.fetchzip {
    url = "https://www.quaddicted.com/filebase/ad_v1_80p1final.zip";
    hash = "sha256-4Iq1Huh2qUDzXv2prx9yF/w548vta2XhBKFiqYXoL8s=";
    stripRoot = false;
  };
  q15Source = pkgs.fetchurl {
    url = "https://archive.org/download/quake15-mirror/Q15_PublicBeta1.7z";
    hash = "sha256-huvBJdBRyxOQf7dufG4JQ+V/0uXdf/PuLmw95OrDuXs=";
  };
  q15Mod = pkgs.stdenv.mkDerivation {
    name = "quake-1-5";
    src = q15Source;
    dontUnpack = true;
    buildInputs = [p7zip];
    installPhase = ''
      mkdir -p $out
      cd $out
      ${p7zip}/bin/7z x -y $src
    '';
  };
  quakeRoot = pkgs.linkFarm "quake-root" [
    {
      name = "id1";
      path = "${librequake}/id1";
    }
    {
      name = "qbj3";
      path = "${qbj3Mod}";
    }
    {
      name = "ad";
      path = "${adMod}";
    }
    {
      name = "ironwail.pak";
      path = "${ironwail}/share/quake/ironwail.pak";
    }
    {
      name = "quake15";
      path = "${q15Mod}/quake15";
    }
  ];
  mkQuakeSand = {
    name,
    text,
  }:
    mkBubble {
      inherit pkgs name text user;
      home = "quake";
      mkOverlay = _: [
        {
          path = ["bubblewrap" "bind" "ro"];
          update = prev: prev ++ [[(toString quakeRoot) "/quake"]];
        }
      ];
    };
  mkQuake = {
    tag,
    engine ? ironwail,
    game ? null,
    args ? "",
  }: {
    "quake-${tag}" = mkQuakeSand {
      name = "quake-${tag}";
      text = ''
        cd /quake
        ${engine}/bin/${engine.pname} -basedir /quake ${
          if game == null
          then ""
          else "-game ${game}"
        } ${args} "$@"
      '';
    };
  };
  games =
    mkQuake {
      tag = "librequake";
    }
    // mkQuake {
      tag = "qbj3";
      game = "qbj3";
    }
    // mkQuake {
      tag = "arcane-dimensions";
      game = "ad";
    }
    // mkQuake {
      tag = "quake-1-5";
      engine = darkplaces;
      game = "quake15";
    };
in
  pkgs.symlinkJoin {
    name = "quake-games";
    paths = pkgs.lib.attrValues games;
  }
