{
  inputs = {
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs:
    inputs.ghc-wasm-meta.inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
      app-watch = pkgs.writeShellApplication rec {
        name = "app-watch";
        text = ''
          ${pkgs.simple-http-server}/bin/simple-http-server -p 3000 ./dist
        '';
      };
      app-ghcid = pkgs.writeScriptBin "app-ghcid" ''
        ${pkgs.ghcid}/bin/ghcid --test="Main.main" --command="${pkgs.cabal-install}/bin/cabal new-repl miso-components-example --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid"
      '';
    in {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}.all_9_10
          app-watch
          app-ghcid
          pkgs.cabal-install
        ];
      };
    });
}
