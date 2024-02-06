with (import ./default.nix); let
  app-ghcid = pkgs.writeScriptBin "app-ghcid" ''
    (cd ${builtins.toString ./.} && ${pkgs.ghcid}/bin/ghcid --test=":main" --command="${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl app --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
  '';
  functora-tools = import "${builtins.trace functora functora}/nix/tools.nix";
  functora-pkgs = import "${builtins.trace functora functora}/nix/nixpkgs.nix";
in
  dev.env.overrideAttrs (prev: {
    buildInputs =
      [
        app-ghcid
        app-release-latest
        app-release-stable
        app-release-readme
        pkgs.haskell.packages.ghc865.cabal-install
        functora-pkgs.nodejs
      ]
      ++ prev.buildInputs
      ++ functora-tools;
  })
