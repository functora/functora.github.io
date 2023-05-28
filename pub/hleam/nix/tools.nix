with (import ./../../../nix/project.nix);
with pkgs;
let pkgDir = builtins.toString ./..;
    ghcidHleamMain = writeShellScriptBin "ghcid-hleam-main" ''
      (cd ${pkgDir} && ${ghcid}/bin/ghcid --command="${hpack}/bin/hpack ${pkgDir} && cabal new-repl hleam-exe --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
    '';
    ghcidHleamTest = writeShellScriptBin "ghcid-hleam-test" ''
      (cd ${pkgDir} && ${ghcid}/bin/ghcid --test=":main --fail-fast --color -f failed-examples" --command="${hpack}/bin/hpack ${pkgDir} && cabal new-repl hleam-test --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
    '';
in
[
  ghcidHleamMain
  ghcidHleamTest
]
