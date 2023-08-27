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
  ormoluUnst = unst.writeShellApplication {
    name = "ormolu";
    text = ''
      ${unst.haskellPackages.fourmolu_0_13_0_0}/bin/fourmolu \
        --indentation 2 \
        --column-limit 80 \
        --function-arrows trailing \
        --comma-style trailing \
        --import-export-style trailing \
        --indent-wheres true \
        --record-brace-space true \
        --newlines-between-decls 1 \
        --haddock-style single-line \
        --haddock-style-module single-line \
        --let-style inline \
        --in-style right-align \
        --single-constraint-parens always \
        --unicode never \
        --respectful false \
        "$@"
    '';
  };
  ormoluTest = pkgs.writeShellScriptBin "ormolu-test" ''
    ${ormoluUnst}/bin/ormolu --mode check \
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
  ormoluUnst
  styleTest
]
