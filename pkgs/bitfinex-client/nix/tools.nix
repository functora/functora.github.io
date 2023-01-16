with (import ./../../../nix/project.nix);
with pkgs;
let pkgDir = builtins.toString ./..;
    ghcidBfx = writeShellScriptBin "ghcid-bfx" ''
      (cd ${pkgDir} && ${ghcid}/bin/ghcid --test=":main --fail-fast --color -f failed-examples" --command="${hpack}/bin/hpack ${pkgDir} && cabal new-repl test:bitfinex-client-test --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
    '';
in
[
  ghcidBfx
]
