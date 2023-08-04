with (import ./project.nix);
with pkgs; let
  unst = import ./nixpkgs-unstable.nix;
  repoDir = builtins.toString ./..;
  hlintTest = pkgs.writeShellScriptBin "hlint-test" ''
    ${pkgs.hlint}/bin/hlint ${repoDir} \
      -X QuasiQuotes \
      --ignore-glob=${repoDir}/frk \
      --ignore-glob=${repoDir}/pub/vi \
      --ignore-glob=${repoDir}/pub/dazzle/test
  '';
  ormoluTest = pkgs.writeShellScriptBin "ormolu-test" ''
    ${unst.ormolu}/bin/ormolu --mode check \
      $( find ${repoDir}/* \( \
         -path '${repoDir}/frk' \
         -o -path '${repoDir}/pub/vi' \
         -o -path '${repoDir}/*/*/dist-newstyle' \
         -o -path '${repoDir}/dist-newstyle' \) -prune \
         -o -name '*.hs' -print )
  '';
  styleTest = pkgs.writeShellScriptBin "style-test" ''
    set -euo pipefail
    ${hlintTest}/bin/hlint-test
    ${ormoluTest}/bin/ormolu-test
  '';
in [
  styleTest
]
