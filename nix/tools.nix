with (import ./project.nix);
with pkgs;
let repoDir = builtins.toString ./..;
    hlintTest = pkgs.writeShellScriptBin "hlint-test" ''
      ${pkgs.hlint}/bin/hlint ${repoDir} --ignore-glob=${repoDir}/pkgs/vi
    '';
    ormoluTest = pkgs.writeShellScriptBin "ormolu-test" ''
      ${pkgs.ormolu}/bin/ormolu --mode check \
        $( find ${repoDir}/* \( \
           -path '${repoDir}/pkgs/vi' \
           -o -path '${repoDir}/dist-newstyle' \) -prune \
           -o -name '*.hs' -print )
    '';
    styleTest = pkgs.writeShellScriptBin "style-test" ''
      set -euo pipefail
      ${hlintTest}/bin/hlint-test
      ${ormoluTest}/bin/ormolu-test
    '';
in
[
  styleTest
]
