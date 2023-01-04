with (import ./../../../nix/project.nix);
with pkgs;
let pkgDir = builtins.toString ./..;
    ghcidHakyll = writeShellScriptBin "ghcid-hakyll" ''
      (cd ${pkgDir} && ${ghcid}/bin/ghcid --test=":main" --command="cabal new-repl site watch --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j")
    '';
in
[
  ghcidHakyll
]
