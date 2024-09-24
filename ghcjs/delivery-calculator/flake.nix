{
  inputs = {
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs:
    inputs.ghc-wasm-meta.inputs.flake-utils.lib.eachDefaultSystem (system: let
      def = import ./default.nix;
      vsn = def.vsn;
      label = def.label;
      pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
      shell = import ./../../nix/shell.nix;
      app-ghcid = pkgs.writeScriptBin "app-ghcid" ''
        ${pkgs.ghcid}/bin/ghcid --test="Main.main" --command="${pkgs.cabal-install}/bin/cabal new-repl ${label} --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid"
      '';
      app-serve-latest = pkgs.writeShellApplication rec {
        name = "app-serve-latest";
        text = ''
          ${pkgs.simple-http-server}/bin/simple-http-server \
            --port 8080 \
            ./dist/latest
        '';
      };
      app-release-wasm = pkgs.writeShellApplication {
        name = "app-release-wasm";
        runtimeInputs = [inputs.ghc-wasm-meta.packages.${system}.all_9_10];
        text = ''
          out="./dist/wasm"
          rm -rf "$out"
          mkdir -p "$out"
          wasm32-wasi-cabal update
          wasm32-wasi-cabal build ${label}
          hs_wasm_path=$(find ../../dist-newstyle -name "${label}.wasm")
          "$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
                --input "$hs_wasm_path" \
                --output "$out/ghc_wasm_jsffi.js"
          wizer --allow-wasi \
                --wasm-bulk-memory true \
                --init-func _initialize \
                -o "$out/bin.wasm" "$hs_wasm_path"
          wasm-opt -O4 "$out/bin.wasm" -o "$out/bin.wasm"
          wasm-tools strip -o "$out/bin.wasm" "$out/bin.wasm"
        '';
      };
      app-release-latest = pkgs.writeShellApplication {
        name = "app-release-latest";
        text = ''
          ${app-release-wasm}/bin/app-release-wasm
          out="./dist/latest"
          rm -rf "$out"
          mkdir -p "$out/static"
          cp ./static/*.png $out/static/
          cp ./static/*.woff2 $out/static/
          cp ./static/*.webmanifest $out/
          cp ./static/*.ico $out/
          # cp ./static/wasm.js $out/
          cp ./dist/wasm/* $out/

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
            ./static/index.html

          ${pkgs.clean-css-cli}/bin/cleancss \
            -O2 \
            --source-map \
            -o $out/static/all.css \
            ./static/material-components-web.min.css \
            ./static/material-icons.css \
            ./static/app.css

          ${pkgs.closurecompiler}/bin/closure-compiler \
            --jscomp_off=checkVars \
            --compilation_level SIMPLE_OPTIMIZATIONS \
            --output_wrapper "%output%//# sourceMappingURL=all.js.map" \
            --create_source_map $out/all.js.map \
            --js ${./static/wasm.js} \
            --js ${./../miso-widgets/js/main.min.js} \
            --js ${./../miso-components/material-components-web.min.js} \
            --js ${./../miso-components/material-components-web-elm.min.js} \
            --js ${./static/app.js} \
            --js_output_file $out/all.js
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
    in {
      devShells.default = pkgs.mkShell {
        packages =
          shell.ghc.all
          ++ shell.buildInputs
          ++ shell.nativeBuildInputs
          ++ [
            pkgs.libwebp
            pkgs.secp256k1
            pkgs.pkg-config
            inputs.ghc-wasm-meta.packages.${system}.all_9_10
            app-ghcid
            app-serve-latest
            app-release-wasm
            app-release-latest
            app-release-apk
            app-release-aab
          ]
          ++ (import ../../nix/tools.nix);
      };
    });
}
