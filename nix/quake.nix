{
  pkgs ? import <nixpkgs> {},
  user ? "quake",
}: let
  mkBubble = import ./bubble.nix;
  ironwail = pkgs.ironwail;
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
  ];
  mkQuakeSand = {
    name,
    text,
  }:
    mkBubble {
      inherit pkgs name text;
      inherit user;
      runtimeInputs = [ironwail];
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
    game ? null,
    args ? "",
  }: {
    "quake-${tag}" = mkQuakeSand {
      name = "quake-${tag}";
      text = ''
        cd /quake
        ${ironwail}/bin/ironwail -basedir /quake ${
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
    };
in
  pkgs.symlinkJoin {
    name = "quake-games";
    paths = pkgs.lib.attrValues games;
  }
