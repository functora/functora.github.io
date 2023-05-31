with (import ./../../../nix/project.nix);
with pkgs;
let pkgDir = builtins.toString ./..;
    ghcidSombraMain = writeShellScriptBin "ghcid-sombra-main" ''
      (cd ${pkgDir} && ${ghcid}/bin/ghcid --command="${hpack}/bin/hpack ${pkgDir} && cabal new-repl sombra-exe --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
    '';
    ghcidSombraTest = writeShellScriptBin "ghcid-sombra-test" ''
      (cd ${pkgDir} && ${ghcid}/bin/ghcid --test=":main --fail-fast --color -f failed-examples" --command="${hpack}/bin/hpack ${pkgDir} && cabal new-repl sombra-test --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
    '';
in
[
  ghcidSombraMain
  ghcidSombraTest
]
