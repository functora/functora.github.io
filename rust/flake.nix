{
  description = "Rust Dev Shell";

  inputs = {
    stable.url = "github:nixos/nixpkgs?ref=nixos-26.05";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    opencode-nix.url = "github:dominicnunez/opencode-nix";
  };

  outputs = {
    self,
    stable,
    rust-overlay,
    flake-utils,
    opencode-nix,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import stable {
          inherit system;
          overlays = [rust-overlay.overlays.default];
          config.android_sdk.accept_license = true;
        };
        mobile-targets = [
          "i686-linux-android"
          "x86_64-linux-android"
          "thumbv7neon-linux-androideabi"
          "armv7-linux-androideabi"
          "aarch64-linux-android"
        ];
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          targets =
            [
              "wasm32-unknown-unknown"
            ]
            ++ mobile-targets;
        };
        wasm-bindgen-cli-0_2_127 = with pkgs;
          rustPlatform.buildRustPackage rec {
            pname = "wasm-bindgen-cli";
            version = "0.2.127";
            src = pkgs.fetchCrate {
              pname = "wasm-bindgen-cli";
              version = "0.2.127";
              sha256 = "sha256-di+qBAdd7pENLiIB9CoZoab+W5xeDoByMREcCGTSzWo=";
            };
            cargoLock.lockFile = "${src}/Cargo.lock";
            nativeBuildInputs = [pkg-config];
            buildInputs =
              [openssl]
              ++ lib.optionals stdenv.hostPlatform.isDarwin [curl];
            nativeCheckInputs = [nodejs_latest];
            doCheck = false;
          };
        android-sdk-args = {
          platformVersions = ["34" "35"];
          buildToolsVersions = ["34.0.0" "35.0.0"];
          abiVersions = ["armeabi-v7a" "arm64-v8a" "x86" "x86_64"];
          systemImageTypes = ["default" "google_apis_playstore"];
          includeNDK = true;
        };
        android-sdk =
          (pkgs.androidenv.composeAndroidPackages android-sdk-args).androidsdk;
        dioxusCli08 = pkgs.rustPlatform.buildRustPackage (finalAttrs: {
          pname = "dioxus-cli";
          version = "0.8.0-alpha.1";
          src = pkgs.fetchCrate {
            pname = "dioxus-cli";
            version = "0.8.0-alpha.1";
            hash = "sha256-4x9xTc9FW03ohEhDOe+wJ0EJ4yR8HWFmiEA+hvlLF7Q=";
          };
          cargoLock.lockFile = "${finalAttrs.src}/Cargo.lock";
          buildFeatures = [
            "no-downloads"
            "disable-telemetry"
          ];
          env = {
            OPENSSL_NO_VENDOR = 1;
          };
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.cacert
            pkgs.installShellFiles
            pkgs.makeWrapper
          ];
          buildInputs = [
            pkgs.openssl
          ];
          nativeCheckInputs = [
            pkgs.rustfmt
          ];
          checkFlags = [
            "--skip=serve::proxy::test"
            "--skip=test_harnesses::run_harness"
          ];
          postInstall = ''
            installShellCompletion --cmd dx \
              --bash <($out/bin/dx completions bash) \
              --fish <($out/bin/dx completions fish) \
              --zsh <($out/bin/dx completions zsh)
          '';
          postFixup = ''
            wrapProgram $out/bin/dx \
              --suffix PATH : ${
              pkgs.lib.makeBinPath [
                pkgs.esbuild
                pkgs.wasm-bindgen-cli_0_2_118
              ]
            }
          '';
        });
        mkAab = app: let
          mkCmd = target: ''
            android-icons
            RES="./target/dx/${app}/release/android/app/app/src/main/res"
            rm -f "$RES"/mipmap-*/ic_launcher.webp \
              "$RES"/mipmap-*/ic_launcher.png \
              "$RES"/mipmap-anydpi-v26/ic_launcher.xml \
              "$RES"/drawable/ic_launcher_background.xml \
              "$RES"/drawable/ic_launcher_background.png \
              "$RES"/drawable-v24/ic_launcher_foreground.xml \
              "$RES"/drawable-v24/ic_launcher_foreground.png
            rm -f ./target/dx/${app}/release/android/app/app/src/main/kotlin/dev/dioxus/main/ProxyActivity.kt
            dx bundle --release --android --debug-symbols=false --target "${target}"
            VSN="$(grep '^version' Cargo.toml | head -1 | sed -E 's/.*"([^"]+)".*/\1/')"
            VC="$(echo "$VSN" | awk -F. '{print $1*10000 + $2*100 + $3}')"
            GRADLE="./target/dx/${app}/release/android/app/app/build.gradle.kts"
            sed -i "s/versionCode = 1/versionCode = $VC/" "$GRADLE"
            KOTLIN="./target/dx/${app}/release/android/app/app/src/main/kotlin/dev/dioxus/main/MainActivity.kt"
            if [ -f ./android-overlay/app/src/main/kotlin/dev/dioxus/main/MainActivity.kt ]; then
              cp ./android-overlay/app/src/main/kotlin/dev/dioxus/main/MainActivity.kt "$KOTLIN"
            fi
            if [ -f ./android-overlay/app/src/main/kotlin/dev/dioxus/main/ProxyActivity.kt ]; then
              cp ./android-overlay/app/src/main/kotlin/dev/dioxus/main/ProxyActivity.kt \
                ./target/dx/${app}/release/android/app/app/src/main/kotlin/dev/dioxus/main/ProxyActivity.kt
            fi
            CHROME="./target/dx/${app}/release/android/app/app/src/main/kotlin/dev/dioxus/main/RustWebChromeClient.kt"
            sed -i 's|val intent = fileChooserParams.createIntent()|val intent = fileChooserParams.createIntent().apply { action = Intent.ACTION_OPEN_DOCUMENT }|' "$CHROME"
            rm -f "$RES"/mipmap-*/ic_launcher.webp \
              "$RES"/mipmap-anydpi-v26/ic_launcher.xml \
              "$RES"/drawable/ic_launcher_background.xml \
              "$RES"/drawable-v24/ic_launcher_foreground.xml
            cp assets/favicon/mipmap-mdpi.png "$RES/mipmap-mdpi/ic_launcher.png"
            cp assets/favicon/mipmap-hdpi.png "$RES/mipmap-hdpi/ic_launcher.png"
            cp assets/favicon/mipmap-xhdpi.png "$RES/mipmap-xhdpi/ic_launcher.png"
            cp assets/favicon/mipmap-xxhdpi.png "$RES/mipmap-xxhdpi/ic_launcher.png"
            cp assets/favicon/mipmap-xxxhdpi.png "$RES/mipmap-xxxhdpi/ic_launcher.png"
            MANIFEST="./target/dx/${app}/release/android/app/app/src/main/AndroidManifest.xml"
            sed -i 's|</activity>|  <intent-filter android:autoVerify="true">\n    <action android:name="android.intent.action.VIEW" />\n    <category android:name="android.intent.category.DEFAULT" />\n    <category android:name="android.intent.category.BROWSABLE" />\n    <data android:scheme="https" android:host="functora.github.io" android:pathPrefix="/apps/${app}/" />\n  </intent-filter>\n</activity>|' "$MANIFEST"
            sed -i 's| android:launchMode="[^"]*"||g' "$MANIFEST"
            sed -i 's|android:name="dev.dioxus.main.MainActivity"|android:name="dev.dioxus.main.MainActivity" android:launchMode="singleInstance"|' "$MANIFEST"
            sed -i 's|</activity>|</activity>\n    <activity android:name="dev.dioxus.main.ProxyActivity" android:exported="true" android:noHistory="true" android:excludeFromRecents="true" android:taskAffinity="" android:theme="@android:style/Theme.Translucent.NoTitleBar">\n      <intent-filter>\n        <action android:name="android.intent.action.VIEW" />\n        <category android:name="android.intent.category.DEFAULT" />\n        <data android:scheme="content" />\n        <data android:scheme="file" />\n        <data android:mimeType="application/octet-stream" />\n        <data android:pathPattern=".*\\.cryptonote" />\n      </intent-filter>\n      <intent-filter>\n        <action android:name="android.intent.action.VIEW" />\n        <category android:name="android.intent.category.DEFAULT" />\n        <data android:scheme="content" />\n        <data android:scheme="file" />\n        <data android:mimeType="*/*" />\n        <data android:pathPattern=".*\\.cryptonote" />\n      </intent-filter>\n      <intent-filter>\n        <action android:name="android.intent.action.SEND" />\n        <category android:name="android.intent.category.DEFAULT" />\n        <data android:mimeType="application/octet-stream" />\n      </intent-filter>\n    </activity>|' "$MANIFEST"
            export ANDROID_HOME="${android-sdk}/libexec/android-sdk"
            export GRADLE_OPTS="-Dorg.gradle.project.android.aapt2FromMavenOverride=${android-sdk}/libexec/android-sdk/build-tools/35.0.0/aapt2"
            (cd "./target/dx/${app}/release/android/app" && ./gradlew bundleRelease)
            OUT="./target/dx/${app}/release/android/app/app/build/outputs/bundle/release"
            cp "$OUT/app-release.aab" "$OUT/cryptonote-v$VSN-${target}.aab"
            mv "$OUT/app-release.aab" "$OUT/cryptonote-v$VSN.aab"
            echo "Aab ${app} release success for ${target}!"
          '';
        in
          (
            map (
              target:
                pkgs.writeShellApplication {
                  name = "release-aab-${app}-${target}";
                  runtimeInputs = with pkgs; [coreutils gnugrep gnused jdk];
                  text = ''
                    (
                      cd "${app}"
                      ${mkCmd target}
                    )
                  '';
                }
            )
            mobile-targets
          )
          ++ [
            (
              pkgs.writeShellApplication {
                name = "release-aab-${app}-all";
                runtimeInputs = with pkgs; [coreutils gnugrep gnused jdk];
                text = ''
                  (
                    cd "${app}"
                    ${builtins.concatStringsSep "\n" (map mkCmd mobile-targets)}
                  )
                '';
              }
            )
          ];
        mkWeb = app:
          pkgs.writeShellApplication rec {
            name = "release-web-${app}";
            runtimeInputs = with pkgs; [coreutils gnugrep gnused];
            text = ''
              (
                cd "${app}"
                VSN="$(grep '^version' Cargo.toml | head -1 | sed -E 's/.*"([^"]+)".*/\1/')"
                REL="../../apps/${app}/$VSN"
                if [ -d "$REL" ]
                then
                  echo "$REL does already exist!"
                  exit 1
                else
                  mkdir -p "$REL"
                fi
                dx bundle --release --web --debug-symbols=false
                cp ../functora-dioxus/assets/sw.js ./target/dx/${app}/release/web/public/sw.js
                cp -R ./target/dx/${app}/release/web/public/* "$REL"
                echo "<!doctype html><html><head><meta http-equiv=\"Refresh\" content=\"0; url=$VSN\"></head><body></body></html>" > ../../apps/${app}/index.html
                echo "$REL web release success!"
              )
            '';
          };
        srWeb = app:
          pkgs.writeShellApplication {
            name = "serve-web-${app}";
            runtimeInputs = with pkgs; [python3];
            text = ''
                  (
                    cd "${app}"
                    VSN="$(grep '^version' Cargo.toml | head -1 | sed -E 's/.*"([^"]+)".*/\1/')"
                    dx bundle --release --web --debug-symbols=false
                    cp ../functora-dioxus/assets/sw.js ./target/dx/${app}/release/web/public/sw.js
                    python3 <<PYEOF
              import http.server, socketserver, os
              PORT = 8000
              BASE = "/apps/${app}/$VSN"
              ROOT = os.path.abspath("./target/dx/${app}/release/web/public")

              class Handler(http.server.SimpleHTTPRequestHandler):
                  def __init__(self, *a, **k):
                      super().__init__(*a, directory=ROOT, **k)
                  def do_GET(self):
                      if self.path.startswith(BASE):
                          self.path = self.path[len(BASE):] or "/"
                      else:
                          self.send_error(404)
                          return
                      super().do_GET()

              socketserver.TCPServer(("", PORT), Handler).serve_forever()
              PYEOF
                  )
            '';
          };
        android-keygen = pkgs.writeShellApplication {
          name = "android-keygen";
          text = ''
            if [ ! -f ~/keys/app-key.jks ]; then
              mkdir -p ~/keys
              ${pkgs.zulu}/bin/keytool -genkey -v \
                -keystore ~/keys/app-key.jks \
                -keyalg RSA \
                -keysize 2048 \
                -validity 10000 \
                -alias app-key
            fi
          '';
        };
        android-icons = pkgs.writeShellApplication {
          name = "android-icons";
          runtimeInputs = [pkgs.imagemagick];
          text = ''
            INPUT="assets/favicon/android-chrome-512x512.png"
            DIR="assets/favicon"

            magick "$INPUT" -resize 48x48 "$DIR/mipmap-mdpi.png"
            magick "$INPUT" -resize 72x72 "$DIR/mipmap-hdpi.png"
            magick "$INPUT" -resize 96x96 "$DIR/mipmap-xhdpi.png"
            magick "$INPUT" -resize 144x144 "$DIR/mipmap-xxhdpi.png"
            magick "$INPUT" -resize 192x192 "$DIR/mipmap-xxxhdpi.png"
          '';
        };
        release-assetlinks-json = pkgs.writeShellApplication {
          name = "release-assetlinks-json";
          runtimeInputs = with pkgs; [coreutils gnugrep gnused jdk];
          text = ''
            KEYSTORE="$HOME/keys/app-key.jks"
            DIR="''${1:-../pub/functora-hakyll}"
            shift 2>/dev/null || true

            APPS=("$@")
            if [ ''${#APPS[@]} -eq 0 ]; then
              APPS=(cryptonote)
            fi

            if [ ! -f "$KEYSTORE" ]; then
              echo "Keystore not found at $KEYSTORE. Run android-keygen first."
              exit 1
            fi
            IFS= read -r -s -p "Keystore password: " KS_PASS
            echo
            FP="$(keytool -list -v -keystore "$KEYSTORE" -alias app-key -storepass "$KS_PASS" 2>/dev/null | grep "SHA256:" | awk '{print $NF}')"
            if [ -z "$FP" ]; then
              echo "Failed to extract SHA256 fingerprint"
              exit 1
            fi
            mkdir -p "$DIR/.well-known"
            {
              printf '[\n'
              for i in "''${!APPS[@]}"; do
                [ "$i" -gt 0 ] && printf ',\n'
                printf '  {\n    "relation": ["delegate_permission/common.handle_all_urls"],\n    "target": {\n      "namespace": "android_app",\n      "package_name": "com.functora.%s",\n      "sha256_cert_fingerprints": ["%s", "6F:FA:9F:54:93:B0:CA:76:D5:0E:0A:5B:41:84:3A:7B:6E:F4:25:8B:AB:8C:23:13:98:76:D9:E8:AC:06:F6:2D"]\n    }\n  }\n' "''${APPS[$i]}" "$FP"
              done
              printf ']\n'
            } > "$DIR/.well-known/assetlinks.json"
            echo "Wrote $DIR/.well-known/assetlinks.json"
          '';
        };
        mkApk = app:
          pkgs.writeShellApplication {
            name = "release-apk-${app}";
            text = ''
              DEF="./${app}/target/dx/${app}/release/android/app/app/build/outputs/bundle/release"
              DIR="''${1:-$DEF}"

              IFS= read -r -s -p "Keystore password: " KS_PASS
              echo

              export BUNDLETOOL_AAPT2_PATH="${android-sdk}/libexec/android-sdk/build-tools/35.0.0/aapt2";
              export JAVA_TOOL_OPTIONS="-Daapt2Path=$BUNDLETOOL_AAPT2_PATH"
              export BUNDLETOOL_AAPT2="$BUNDLETOOL_AAPT2_PATH"

              for AAB in "$DIR"/*.aab; do
                [ -f "$AAB" ] || continue

                NAME=$(${pkgs.coreutils}/bin/basename "$AAB" .aab)
                [[ "$NAME" != *-signed ]] || continue
                SIG="$DIR/$NAME-signed.aab"
                APK="$DIR/$NAME.apk"
                TMP="$DIR/$NAME.apks"

                cp "$AAB" "$SIG"
                "${pkgs.jdk}/bin/jarsigner" -verbose \
                  -keystore "$HOME/keys/app-key.jks" \
                  -storepass "$KS_PASS" \
                  "$SIG" app-key

                "${pkgs.bundletool}/bin/bundletool" build-apks \
                  --bundle="$AAB" \
                  --output="$TMP" \
                  --mode=universal \
                  --aapt2="$BUNDLETOOL_AAPT2_PATH" \
                  --ks="$HOME/keys/app-key.jks" \
                  --ks-pass=pass:"$KS_PASS" \
                  --ks-key-alias=app-key \
                  --overwrite

                "${pkgs.unzip}/bin/unzip" -p "$TMP" universal.apk > "$APK"
                rm -f "$TMP"

                echo "READY: $APK"
              done
            '';
          };
        shell = rec {
          ANDROID_HOME = "${android-sdk}/libexec/android-sdk";
          ANDROID_SDK_ROOT = ANDROID_HOME;
          NDK_HOME = "${ANDROID_HOME}/ndk-bundle";
          ANDROID_NDK_HOME = NDK_HOME;
          GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${android-sdk}/libexec/android-sdk/build-tools/35.0.0/aapt2";
          CARGO_TARGET_X86_64_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/x86_64-linux-android28-clang";
          CARGO_TARGET_AARCH64_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android28-clang";
          CARGO_TARGET_ARMV7_LINUX_ANDROIDEABI_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi28-clang";
          CARGO_TARGET_I686_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/i686-linux-android28-clang";
          packages = with pkgs;
            [
              bacon
              rustToolchain
              rust-analyzer
              cargo-tarpaulin
              clippy
              wasmtime
              license-generator
              dioxusCli08
              tailwindcss_4
              simple-http-server
              strace
              # web
              binaryen
              wasm-bindgen-cli-0_2_127
              lessc
              clean-css-cli
              # linux
              pkg-config
              webkitgtk_4_1
              openssl
              xdotool
              libayatana-appindicator
              librsvg
              gtk3
              gdk-pixbuf
              cairo
              pango
              curl
              wget
              zlib
              fuse
              file
              gcc
              # android
              aapt
              llvmPackages.lld
              llvmPackages.clang-unwrapped
              android-sdk
              glibc
              jdk
              android-icons
              android-keygen
              release-assetlinks-json
              # fonts
              noto-fonts
              noto-fonts-cjk-sans
              noto-fonts-color-emoji
              liberation_ttf
              dejavu_fonts
              # apps
              (mkWeb "cryptonote")
              (srWeb "cryptonote")
              (mkApk "cryptonote")
              # tools
              pkgs.chromium
              pkgs.qutebrowser
              pkgs.cloudflared
              (opencode-nix.packages.${system}.default)
              (pkgs.writeShellApplication {
                name = "verify";
                text = ''
                  verify_crate() {
                    local crate="$1"
                    shift
                    ${cargo}/bin/cargo fmt "$@" \
                      && if [ -f Dioxus.toml ]; then dx fmt "$@"; fi \
                      && ${cargo}/bin/cargo clippy --all-features --all-targets "$@" -- -D warnings \
                      && ${cargo}/bin/cargo test --all-features --all-targets "$@" \
                      && if [ "$crate" = "cryptonote" ]; then
                           for T in ${pkgs.lib.concatStringsSep " " mobile-targets}; do
                             ${cargo}/bin/cargo clippy --target "$T" --all-features --all-targets "$@" -- -D warnings \
                               && echo "==> $crate [$T]: mobile clippy: All good!"
                           done
                         fi
                  }
                  if [ -f Cargo.toml ]; then
                    verify_crate "$(basename "$PWD")" "$@" && echo "==> All good!"
                  else
                    FOUND=0
                    for P in */; do
                      P="''${P%/}"
                      [ -f "$P/Cargo.toml" ] || continue
                      FOUND=1
                      (
                        cd "$P"
                        verify_crate "$P" "$@" && echo "==> $P: All good!"
                      ) || {
                        echo "==> $P: FAILED"
                        exit 1
                      }
                    done
                    if [ "$FOUND" -eq 0 ]; then
                      echo "==> No Rust crate found in this directory!"
                      exit 1
                    fi
                    echo "==> All Rust projects verified!"
                  fi
                '';
              })
              (pkgs.writeShellApplication {
                name = "coverage";
                text = ''
                  cargo tarpaulin --all-features --engine llvm "$@" \
                    && echo "==> Coverage done!"
                '';
              })
              (pkgs.writeShellApplication {
                name = "tunnel-8000";
                text = ''
                  ${pkgs.cloudflared}/bin/cloudflared tunnel --protocol http2 --edge-ip-version 4 --url http://localhost:8000
                '';
              })
            ]
            ++ (mkAab "cryptonote");
        };
        mkRustPkg = pkg:
          pkgs.rustPlatform.buildRustPackage {
            name = pkg;
            src = pkgs.nix-gitignore.gitignoreSource [] ./${pkg};
            cargoLock.lockFile = ./${pkg}/Cargo.lock;
          };
        mkRustPkgWasm = pkg: let
          buildTarget = "wasm32-wasip1";
          pkgs = import stable {
            inherit system;
            overlays = [rust-overlay.overlays.default];
          };
          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            targets = [buildTarget];
          };
          rustPlatform = pkgs.makeRustPlatform {
            cargo = rustToolchain;
            rustc = rustToolchain;
          };
        in
          rustPlatform.buildRustPackage {
            name = pkg;
            src = pkgs.nix-gitignore.gitignoreSource [] ./${pkg};
            cargoLock.lockFile = ./${pkg}/Cargo.lock;
            buildPhase = ''
              cargo build --release -p ${pkg} --target=${buildTarget}
            '';
            installPhase = ''
              mkdir -p $out/lib
              cp target/${buildTarget}/release/*.wasm $out/lib/
            '';
          };
        mkRustellNvim = exe:
          pkgs.vimUtils.buildVimPlugin {
            name = "rustell-nvim";
            src = pkgs.writeTextDir "plugin/rustell.vim" ''
              augroup rust.vim.PreWrite
                autocmd!
              augroup END

              augroup rustell_PreWrite
                autocmd!
                autocmd BufWritePre *.rs call s:RustellPreWrite()
              augroup END

              function! s:RustellPreWrite()
                if !filereadable(expand('%'))
                  return
                endif

                " Read buffer
                let l:input = join(getline(1, '$'), "\n")

                " --- run rustell first ---
                let l:rustell_out = system('${exe}', l:input)
                if v:shell_error
                  echohl ErrorMsg | echom 'rustell failed: ' . l:rustell_out | echohl None
                  return
                endif

                " --- now feed result into rustfmt ---
                let l:rustfmt_cmd = 'rustfmt'
                if exists('g:rustfmt_command')
                  let l:rustfmt_cmd = g:rustfmt_command
                endif
                if exists('g:rustfmt_options')
                  let l:rustfmt_cmd .= ' ' . g:rustfmt_options
                endif

                let l:rustfmt_out = system(l:rustfmt_cmd, l:rustell_out)
                if v:shell_error
                  echohl ErrorMsg | echom 'rustfmt failed: ' . l:rustfmt_out | echohl None
                  return
                endif

                " Replace buffer with formatted output
                let l:out_lines = split(l:rustfmt_out, "\n")
                call setline(1, l:out_lines)
                if line('$') > len(l:out_lines)
                  execute len(l:out_lines)+1 . ',$delete _'
                endif
              endfunction
            '';
          };
      in {
        devShells.default = pkgs.mkShell shell;
        packages = rec {
          rustell = mkRustPkg "rustell";
          rustell-nvim = mkRustellNvim "${rustell}/bin/rustell";
          rustell-wasm = mkRustPkgWasm "rustell";
          rustell-wasm-nvim = mkRustellNvim "${
            pkgs.wasmtime
          }/bin/wasmtime ${rustell-wasm}/lib/rustell.wasm";
          dioxus-cli-08 = dioxusCli08;
          default = self.packages.${system}.rustell;
        };
      }
    );
}
