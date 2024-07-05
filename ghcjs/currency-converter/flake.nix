{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      overlays = [
        haskellNix.overlay
        (final: _prev: {
          hixProject = final.haskell-nix.hix.project {
            src = ./../..;
            # uncomment with your current system for `nix flake show` to work:
            evalSystem = "x86_64-linux";
            modules = [
              {
                doCheck = false;
                doCrossCheck = false;
                doHaddock = false;
                doHoogle = false;
              }
            ];
            compiler-nix-name = "ghc911";
            crossPlatforms = p: [p.ghcjs];
            shell.tools.cabal = "latest";
            shell.buildInputs =
              [
                pkgs.ghc
                pkgs.nodejs-slim
                pkgs.pkg-config
                pkgs.libwebp
                app-ghcid
                app-serve-latest
                app-release-latest
                app-release-apk
                app-release-aab
              ]
              ++ (import ../../nix/tools.nix);
          };
        })
      ];
      app-ghcid = pkgs.writeScriptBin "app-ghcid" ''
        ${pkgs.ghcid}/bin/ghcid --test="Main.main" --command="${pkgs.cabal-install}/bin/cabal new-repl app --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid"
      '';
      app-serve-latest = pkgs.writeShellApplication rec {
        name = "app-serve-latest";
        text = ''
          ${pkgs.simple-http-server}/bin/simple-http-server \
            -p 8080 ./dist/latest
        '';
      };
      app-release-latest = pkgs.writeShellApplication {
        name = "app-release-latest";
        text = ''
          javascript-unknown-ghcjs-cabal build
          out="./dist/latest"
          rm -rf "$out"
          mkdir -p "$out/static"
          cp ./static/*.png $out/static/
          cp ./static/*.woff2 $out/static/
          cp ./static/*.webmanifest $out/
          cp ./static/*.ico $out/

          echo "# [Back](index.html)" > "$out/index-link.md"
          ${pkgs.pandoc}/bin/pandoc \
            --standalone \
            --from markdown \
            --metadata title="LICENSE" \
            ./LICENSE "$out/index-link.md" > "$out/license.html"
          ${pkgs.pandoc}/bin/pandoc \
            --standalone \
            --from markdown \
            --metadata title="PRIVACY POLICY" \
            ./privacy.md "$out/index-link.md" > "$out/privacy.html"
          rm "$out/index-link.md"

          ${pkgs.html-minifier}/bin/html-minifier \
            --minify-js \
            --minify-css \
            -o $out/license.html \
            $out/license.html
          ${pkgs.html-minifier}/bin/html-minifier \
            --minify-js \
            --minify-css \
            -o $out/privacy.html \
            $out/privacy.html
          ${pkgs.html-minifier}/bin/html-minifier \
            --minify-js \
            --minify-css \
            -o $out/index.html \
            ./static/ghcjs.html

          ${pkgs.clean-css-cli}/bin/cleancss \
            -O2 \
            --source-map \
            -o $out/static/all.css \
            ./static/material-components-web.min.css \
            ./static/material-icons.css \
            ./static/app.css

          app_jsexe_path=$(find ../../dist-newstyle -name "app.jsexe")

          #
          # TODO : fix this!
          # At the moment it gives division by zero runtime exception!
          #

          cp "$app_jsexe_path/all.js" $out/all.js

          # ${pkgs.closurecompiler}/bin/closure-compiler \
          #   --jscomp_off=checkVars \
          #   --compilation_level ADVANCED_OPTIMIZATIONS \
          #   --externs "$app_jsexe_path/all.js.externs" \
          #   --externs ./static/app.js \
          #   --externs ./static/hashable.js \
          #   --externs ./static/material-components-web.min.js \
          #   --externs ./static/material-components-web-elm.min.js \
          #   --output_wrapper "%output%//# sourceMappingURL=all.js.map" \
          #   --create_source_map $out/all.js.map \
          #   --js "$app_jsexe_path/all.js" \
          #   --js_output_file $out/all.js
        '';
      };
      app-release-apk = pkgs.writeShellApplication {
        name = "app-release-apk";
        text = ''
          ${app-release-latest}/bin/app-release-latest
          nix-shell ./android.nix --command "app-release-apk"
        '';
      };
      app-release-aab = pkgs.writeShellApplication {
        name = "app-release-aab";
        text = ''
          ${app-release-latest}/bin/app-release-latest
          nix-shell ./android.nix --command "app-release-aab"
        '';
      };
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.hixProject.flake {};
    in
      flake
      // {
        legacyPackages = pkgs;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
