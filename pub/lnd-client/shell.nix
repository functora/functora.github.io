{
  extraBuildInputs ? [],
  withShellHook ? true,
  profile ? false,
}: let
  # project = import ./default.nix {inherit profile;};
  # header = import ./nix/header.nix;
  # haskellPackages = header.haskellPackages;
  # pkgs = header.pkgs;
  # nixPkgsLegacy = header.nixPkgsLegacy;
  # lnd = import ./nix/lnd.nix {inherit pkgs;};
  proto-lens-protoc = import ./nix/proto-lens-protoc.nix;
  project = import ../../nix/project.nix {};
  pkgs = project.pkgs;
in
  project.shellFor {
    withHoogle = true;
    buildInputs =
      [
        # nixPkgsLegacy.cabal-install
        # haskellPackages.hspec-discover
        # haskellPackages.fswatcher
        # haskellPackages.hpack
        # haskellPackages.stack
        # haskellPackages.hp2pretty
        # pkgs.ormolu
        # pkgs.niv
        pkgs.bitcoin
        pkgs.openssl
        pkgs.expect
        pkgs.ps
        pkgs.jq
        pkgs.lnd
        pkgs.protobuf
        proto-lens-protoc
      ]
      ++ extraBuildInputs;
    tools = {
      ghcid = "latest";
      # hlint = "latest";
      # haskell-language-server = "latest";
    };
    shellHook =
      if withShellHook
      then ''
        echo "Spawning nix-shell with shellHook"
        . ./script/export-test-envs.sh
        trap "./script/cleanup-test-env.sh 2> /dev/null" EXIT
      ''
      else ''
        echo "Spawning nix-shell without shellHook"
      '';
  }
