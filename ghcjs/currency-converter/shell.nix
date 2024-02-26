with (import ./default.nix); let
  repo = toString ./.;
  app-ghcid = pkgs.writeScriptBin "app-ghcid" ''
    (cd ${builtins.toString ./.} && ${pkgs.ghcid}/bin/ghcid --test="Main.main" --command="${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl app --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
  '';
  functora-tools = import "${builtins.trace functora functora}/nix/tools.nix";
  functora-pkgs = import "${builtins.trace functora functora}/nix/nixpkgs.nix";
  app-release-android = functora-pkgs.writeShellApplication {
    name = "app-release-android";
    text = ''
      nix-shell ${repo}/android.nix --command "app-release-android"
    '';
  };
in
  dev.env.overrideAttrs (prev: {
    buildInputs =
      [
        app-ghcid
        app-release-latest
        app-release-stable
        app-release-readme
        app-release-android
        app-publish
        pkgs.haskell.packages.ghc865.cabal-install
      ]
      ++ prev.buildInputs
      ++ functora-tools;
  })
