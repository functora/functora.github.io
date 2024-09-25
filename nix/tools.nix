with (import ./project.nix);
with pkgs; let
  newpkgs = import ./newpkgs.nix;
  repoDir = builtins.toString ./..;
  hlintTest = pkgs.writeShellScriptBin "hlint-test" ''
    ${pkgs.hlint}/bin/hlint ${repoDir} \
      -X QuasiQuotes \
      --ignore-glob=${repoDir}/frk \
      --ignore-glob=${repoDir}/pub/vi \
      --ignore-glob=${repoDir}/pub/dazzle/test
  '';
  ormolu = pkgs.writeShellApplication {
    name = "ormolu";
    text = ''
      ${newpkgs.haskellPackages.fourmolu}/bin/fourmolu \
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
  ormoluFormat = pkgs.writeShellApplication {
    name = "ormolu-format";
    text = ''
      ${ormolu}/bin/ormolu --mode inplace \
        "$(find . -name '*.hs' -not -path './dist*')"
    '';
  };
  ormoluTest = pkgs.writeShellScriptBin "ormolu-test" ''
    ${ormolu}/bin/ormolu --mode check \
      $( find ${repoDir}/* \( \
         -path '${repoDir}/frk' \
         -o -path '${repoDir}/pub/vi' \
         -o -path '${repoDir}/*/*/dist-newstyle' \
         -o -path '${repoDir}/dist-newstyle' \) -prune \
         -o -name '*.hs' -print )
  '';
  prettier = pkgs.writeShellApplication {
    name = "prettier";
    text = ''
      ${
        pkgs.nodePackages.prettier
      }/bin/prettier --plugin ${
        pkgs.nodePackages.prettier-plugin-toml
      }/lib/node_modules/prettier-plugin-toml/lib/api.js "$@"
    '';
  };
  styleTest = pkgs.writeShellScriptBin "style-test" ''
    set -euo pipefail
    ${hlintTest}/bin/hlint-test
    ${ormoluTest}/bin/ormolu-test
  '';
in [
  ormolu
  ormoluFormat
  styleTest
  prettier
  pkgs.alejandra
  newpkgs.haskellPackages.cabal-fmt
  pkgs.haskell-language-server
]
