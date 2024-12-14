with (import ./../../../nix/project.nix {});
with pkgs; let
  pkgDir = builtins.toString ./..;
  ghcidDazzleMain = writeShellScriptBin "ghcid-dazzle-main" ''
    (cd ${pkgDir} && ${ghcid}/bin/ghcid --command="${hpack}/bin/hpack ${pkgDir} && cabal new-repl dazzle-exe --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
  '';
  ghcidDazzleTest = writeShellScriptBin "ghcid-dazzle-test" ''
    (cd ${pkgDir} && ${ghcid}/bin/ghcid --test=":main --fail-fast --color -f failed-examples" --command="${hpack}/bin/hpack ${pkgDir} && cabal new-repl dazzle-test --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
  '';
in [
  ghcidDazzleMain
  ghcidDazzleTest
]
