with (import ./default.nix); let
  app-ghcid = pkgs.writeScriptBin "app-ghcid" ''
    (cd ${builtins.toString ./.} && ${pkgs.ghcid}/bin/ghcid --test=":main" --command="${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl app --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
  '';
  app-repl = pkgs.writeScriptBin "app-repl" ''
    (cd ${builtins.toString ./.} && ${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl)
  '';
in
  dev.env.overrideAttrs (prev: {
    buildInputs =
      [
        app-ghcid
        app-repl
      ]
      ++ prev.buildInputs;
  })
