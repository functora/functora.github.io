{
  description = "Functora Rust development environment and desktop applications";

  inputs = {
    stable.url = "github:nixos/nixpkgs?ref=nixos-26.05";
    unstable.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nix-bundlers = {
      url = "github:NixOS/bundlers";
      inputs.nixpkgs.follows = "stable";
    };
  };

  outputs = {
    self,
    stable,
    unstable,
    rust-overlay,
    flake-utils,
    nix-bundlers,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import stable {
          inherit system;
          overlays = [rust-overlay.overlays.default];
          config = {
            android_sdk.accept_license = true;
            allowUnfree = true;
          };
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
        gtk3-schemas = pkgs.runCommand "gtk3-schemas" { } ''
          mkdir -p "$out/glib-2.0/schemas"
          cp -r ${pkgs.gtk3}/share/gsettings-schemas/*/glib-2.0/schemas/. "$out/glib-2.0/schemas/"
        '';
        mkNixBundle = installable:
          let
            program =
              if installable.type == "app" then
                installable.program
              else
                pkgs.lib.getExe installable;
            app = builtins.unsafeDiscardStringContext (builtins.baseNameOf program);
            target =
              if pkgs.lib.hasAttr app desktopBundleRuntimes then
                desktopBundleRuntimes.${app}
              else
                {
                  type = "derivation";
                  outPath = builtins.dirOf (builtins.dirOf program);
                  pname = app;
                  meta.mainProgram = app;
                };
          in
          nix-bundlers.bundlers.${system}.toArx target;
        android-sdk-args = {
          platformVersions = ["34" "35" "36"];
          buildToolsVersions = ["34.0.0" "35.0.0" "36.0.0"];
          abiVersions = ["armeabi-v7a" "arm64-v8a" "x86" "x86_64"];
          systemImageTypes = ["default" "google_apis_playstore"];
          includeNDK = true;
        };
        android-sdk =
          (pkgs.androidenv.composeAndroidPackages android-sdk-args).androidsdk;
        mkFunctoraEguiWeb = app: icons:
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
                LIBNAME="$(echo "${app}" | tr - _)"
                ${rustToolchain}/bin/cargo build --release --lib --target wasm32-unknown-unknown
                ${wasm-bindgen-cli-0_2_127}/bin/wasm-bindgen \
                  --target web \
                  --no-typescript \
                  --out-dir "$REL/pkg" \
                  "../target/wasm32-unknown-unknown/release/$LIBNAME.wasm"
                ${pkgs.binaryen}/bin/wasm-opt -O2 --strip-debug \
                  "$REL/pkg/''${LIBNAME}_bg.wasm" -o "$REL/pkg/''${LIBNAME}_bg.opt.wasm"
                mv "$REL/pkg/''${LIBNAME}_bg.opt.wasm" "$REL/pkg/''${LIBNAME}_bg.wasm"
                cp assets/index.html "$REL/index.html"
                cp assets/manifest.webmanifest "$REL/manifest.webmanifest"
                cp assets/egui.js "$REL/egui.js"
                cp ../functora-egui/assets/web/sw.js "$REL/sw.js"
                cp "${icons}/android-chrome-192x192.png" "$REL/"
                cp "${icons}/android-chrome-512x512.png" "$REL/"
                cp "${icons}/favicon.ico" "$REL/"
                echo "<!doctype html><html><head><meta http-equiv=\"Refresh\" content=\"0; url=$VSN\"></head><body></body></html>" > ../../apps/${app}/index.html
                echo "$REL web release success!"
              )
            '';
          };
        srFunctoraEguiWeb = app: icons:
          pkgs.writeShellApplication {
            name = "serve-web-${app}";
            runtimeInputs = with pkgs; [coreutils psmisc gnused gnugrep gzip python3];
            text = ''
                  cd "${app}"
                  LIBNAME="$(echo "${app}" | tr - _)"
                    ${rustToolchain}/bin/cargo build --release --lib --target wasm32-unknown-unknown
                    ${wasm-bindgen-cli-0_2_127}/bin/wasm-bindgen \
                      --target web \
                      --no-typescript \
                      --out-dir /tmp/${app}-web/pkg \
                      "../target/wasm32-unknown-unknown/release/$LIBNAME.wasm"
                    for W in /tmp/${app}-web/pkg/*.wasm
                    do
                      if [ -f "$W" ]
                      then
                        gzip -9 -c "$W" > "$W.gz"
                      fi
                    done
                    cp assets/index.html /tmp/${app}-web/index.html
                    cp assets/manifest.webmanifest /tmp/${app}-web/manifest.webmanifest
                    cp assets/egui.js /tmp/${app}-web/egui.js
                    cp ../functora-egui/assets/web/sw.js /tmp/${app}-web/sw.js
                    cp "${icons}/android-chrome-192x192.png" /tmp/${app}-web/
                    cp "${icons}/android-chrome-512x512.png" /tmp/${app}-web/
                    cp "${icons}/favicon.ico" /tmp/${app}-web/
                    fuser -k -TERM 8000/tcp 2>/dev/null || true
                    sleep 0.5
                    exec python3 <<PYEOF
              import http.server, os
              PORT = 8000
              ROOT = "/tmp/${app}-web"

              class Handler(http.server.SimpleHTTPRequestHandler):
                  precompressed = False
                  def __init__(self, *a, **k):
                      super().__init__(*a, directory=ROOT, **k)
                  def send_head(self):
                      head, sep, query = self.path.partition("?")
                      gz = head + ".gz"
                      self.precompressed = (
                          head.endswith(".wasm")
                          and "gzip" in self.headers.get("Accept-Encoding", "")
                          and os.path.isfile(self.translate_path(gz))
                      )
                      if self.precompressed:
                          self.path = gz + sep + query
                      return super().send_head()
                  def guess_type(self, path):
                      return super().guess_type(path[:-3] if self.precompressed else path)
                  def end_headers(self):
                      if self.precompressed:
                          self.send_header("Content-Encoding", "gzip")
                      super().end_headers()

              http.server.ThreadingHTTPServer(("", PORT), Handler).serve_forever()
              PYEOF
            '';
          };
        mkFunctoraEguiDesktop = app:
          let
            manifest = builtins.fromTOML (builtins.readFile (sourceRoot + "/${app}/Cargo.toml"));
          in
          pkgs.writeShellApplication {
            name = "release-desktop-${app}";
            runtimeInputs = with pkgs; [coreutils nixVersions.nix_2_34];
            text = ''
              if [ ! -f flake.nix ]; then
                echo "Run release-desktop-${app} from the rust/ flake directory." >&2
                exit 1
              fi

              OUT="''${1:-target/desktop-release}"
              ARTIFACT="$OUT/${app}-v${manifest.package.version}-${system}.arx"
              BUNDLE="$OUT/.${app}-v${manifest.package.version}-${system}.arx"

              if [ -e "$ARTIFACT" ] || [ -L "$ARTIFACT" ]; then
                echo "$ARTIFACT already exists!" >&2
                exit 1
              fi
              if [ -e "$BUNDLE" ] || [ -L "$BUNDLE" ]; then
                echo "$BUNDLE already exists!" >&2
                exit 1
              fi

              mkdir -p "$OUT"
              trap 'rm -f -- "$BUNDLE"' EXIT
              nix --extra-experimental-features "nix-command flakes" bundle \
                --no-write-lock-file \
                --bundler .# \
                --out-link "$BUNDLE" \
                ".#${app}"
              cp --dereference --preserve=mode "$BUNDLE" "$ARTIFACT"
              chmod +x "$ARTIFACT"
              trap - EXIT
              rm -f -- "$BUNDLE"
              test -x "$ARTIFACT"

              echo "READY: $ARTIFACT"
              echo "Run it with: $ARTIFACT"
              echo "ARX bundles require Linux unprivileged user namespaces."
            '';
          };
        srFunctoraEguiDesktop = app:
          pkgs.writeShellApplication {
            name = "serve-desktop-${app}";
            runtimeInputs = with pkgs; [coreutils];
            text = ''
              cd "${app}"
              exec ${rustToolchain}/bin/cargo run --release -- "$@"
            '';
          };
        sourceRoot = self.outPath;
        mkDesktopPackage = app: let
          manifest = builtins.fromTOML (builtins.readFile (sourceRoot + "/${app}/Cargo.toml"));
        in
          pkgs.rustPlatform.buildRustPackage {
            pname = app;
            version = manifest.package.version;
            src = sourceRoot;
            cargoLock.lockFile = sourceRoot + "/${app}/Cargo.lock";
            cargoRoot = app;
            buildAndTestSubdir = app;
            nativeBuildInputs = with pkgs; [
              pkg-config
              makeWrapper
              desktop-file-utils
              appstream
            ];
            buildInputs = with pkgs; [
              libGL
              wayland
              libxkbcommon
              libx11
              libxcursor
              libxrandr
              libxi
              libxext
              gtk3
              adwaita-icon-theme
            ];
            postInstall = ''
              desktop-file-validate ${app}/desktop/linux/*.desktop
              appstreamcli validate --no-net ${app}/desktop/metainfo/*.metainfo.xml
              appid="$(basename ${app}/desktop/linux/*.desktop .desktop)"
              mkdir -p $out/share/applications $out/share/metainfo $out/share/icons
              cp "${app}/desktop/linux/$appid.desktop" $out/share/applications/
              cp "${app}/desktop/metainfo/$appid.metainfo.xml" $out/share/metainfo/
              cp -R "${app}/desktop/icons/hicolor" $out/share/icons/
              wrapProgram $out/bin/${app} \
                --prefix LD_LIBRARY_PATH : "${
                pkgs.lib.makeLibraryPath (with pkgs; [
                  libGL
                  wayland
                  libxkbcommon
                  libx11
                  libxcursor
                  libxrandr
                  libxi
                  libxext
                  gtk3
                ])
              }" \
                --suffix PATH : "${pkgs.lib.makeBinPath [pkgs.xdg-utils]}" \
                --set GSETTINGS_SCHEMA_DIR "${gtk3-schemas}/glib-2.0/schemas" \
                --set XDG_DATA_DIRS "$out/share:${pkgs.adwaita-icon-theme}/share:/usr/local/share:/usr/share"
            '';
            meta = with pkgs.lib; {
              description = manifest.package.description;
              homepage = manifest.package.repository;
              license = licenses.mit;
              mainProgram = app;
              platforms = platforms.linux;
            };
          };
        desktopPackages = {
          cryptonote = mkDesktopPackage "cryptonote";
          functora-egui-demo = mkDesktopPackage "functora-egui-demo";
        };
        mkDesktopBundleRuntime = app: package:
          let
            mesa = pkgs.mesa;
          in
          pkgs.runCommand "${app}-bundle-runtime" {
            nativeBuildInputs = [pkgs.makeWrapper];
            propagatedBuildInputs = [mesa];
            passthru = {
              pname = app;
              version = package.version;
            };
            meta.mainProgram = app;
          } ''
            mkdir -p "$out/bin" "$out/share/glvnd/glx_vendor.d"
            cat > "$out/share/glvnd/glx_vendor.d/50_mesa.json" <<EOF
            {
              "file_format_version": "1.0.0",
              "ICD": {
                "library_path": "${mesa}/lib/libGLX_mesa.so.0"
              }
            }
            EOF
            makeWrapper ${package}/bin/${app} "$out/bin/${app}" \
              --prefix LD_LIBRARY_PATH : "${mesa}/lib" \
              --set __EGL_VENDOR_LIBRARY_FILENAMES "${mesa}/share/glvnd/egl_vendor.d/50_mesa.json" \
              --set __GLX_VENDOR_LIBRARY_FILENAMES "$out/share/glvnd/glx_vendor.d/50_mesa.json" \
              --set LIBGL_DRIVERS_PATH "${mesa}/lib/dri"
            chmod +x "$out/bin/${app}"
            touch "$out"
          '';
        desktopBundleRuntimes = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          cryptonote = mkDesktopBundleRuntime "cryptonote" desktopPackages.cryptonote;
          functora-egui-demo = mkDesktopBundleRuntime "functora-egui-demo" desktopPackages.functora-egui-demo;
        };
        desktopApps = {
          cryptonote = {
            type = "app";
            program = "${desktopPackages.cryptonote}/bin/cryptonote";
            meta.description = "Cryptonote desktop application";
          };
          functora-egui-demo = {
            type = "app";
            program = "${desktopPackages.functora-egui-demo}/bin/functora-egui-demo";
            meta.description = "Functora Egui Demo desktop application";
          };
        };
        desktopBundles = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          cryptonote = mkNixBundle desktopApps.cryptonote;
          functora-egui-demo = mkNixBundle desktopApps.functora-egui-demo;
        };
        desktopBundleCheck = app: bundle: runtime:
          let
            mesa = pkgs.mesa;
          in
          pkgs.runCommand "${app}-bundle-check" {
            nativeBuildInputs = [
              pkgs.bzip2
              pkgs.coreutils
              pkgs.findutils
              pkgs.gnugrep
              pkgs.gnused
              pkgs.gnutar
              pkgs.jq
            ];
          } ''
            test -s "${bundle}"
            test -x "${bundle}"
            test "$(head -n 1 "${bundle}")" = "#!/bin/sh"

            extraction="$TMPDIR/arx-check"
            mkdir -p "$extraction"
            cd "$extraction"
            "${bundle}" --extract

            store="$extraction/dat/nix/store"
            test -d "$store"

            egl_manifest="$extraction/dat${mesa}/share/glvnd/egl_vendor.d/50_mesa.json"
            glx_manifest="$extraction/dat${runtime}/share/glvnd/glx_vendor.d/50_mesa.json"
            test -f "$egl_manifest" || {
              echo "Missing bundled EGL vendor manifest: $egl_manifest" >&2
              exit 1
            }
            test -f "$glx_manifest" || {
              echo "Missing bundled GLX vendor manifest: $glx_manifest" >&2
              exit 1
            }

            egl_library="$(jq -er '.ICD.library_path' "$egl_manifest")"
            glx_library="$(jq -er '.ICD.library_path' "$glx_manifest")"
            test "$egl_library" = "${mesa}/lib/libEGL_mesa.so.0"
            test "$glx_library" = "${mesa}/lib/libGLX_mesa.so.0"
            test -e "$extraction/dat$egl_library"
            test -e "$extraction/dat$glx_library"

            dri_dir="$extraction/dat${mesa}/lib/dri"
            test -d "$dri_dir" || {
              echo "Missing bundled Mesa DRI drivers: $dri_dir" >&2
              exit 1
            }
            test -n "$(find "$dri_dir" -maxdepth 1 \( -type f -o -type l \) -name '*_dri.so' -print -quit)" || {
              echo "No bundled Mesa DRI drivers found in $dri_dir" >&2
              exit 1
            }

            wrapper="$(find "$store" -path "*/bin/${app}" -type f -exec grep -l '__EGL_VENDOR_LIBRARY_FILENAMES' {} \; -quit)"
            test -n "$wrapper" || {
              echo "Missing Mesa-aware bundle wrapper for $app" >&2
              exit 1
            }

            grep -F '__EGL_VENDOR_LIBRARY_FILENAMES' "$wrapper" | grep -F "${mesa}/share/glvnd/egl_vendor.d/50_mesa.json"
            grep -F '__GLX_VENDOR_LIBRARY_FILENAMES' "$wrapper" | grep -F "${runtime}/share/glvnd/glx_vendor.d/50_mesa.json"
            grep -F 'LIBGL_DRIVERS_PATH' "$wrapper" | grep -F "${mesa}/lib/dri"
            grep -F 'LD_LIBRARY_PATH' "$wrapper" | grep -F "${mesa}/lib"

            touch "$out"
          '';
        mkFunctoraEguiAab = app: icons: let
          abis = {
            "aarch64-linux-android" = "arm64-v8a";
            "armv7-linux-androideabi" = "armeabi-v7a";
            "i686-linux-android" = "x86";
            "x86_64-linux-android" = "x86_64";
          };
          targets = builtins.attrNames abis;
          libName = builtins.replaceStrings ["-"] ["_"] app;
          ndk-bin = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin";
          rustEnv = ''
            export ANDROID_HOME="${android-sdk}/libexec/android-sdk"
            export CARGO_TARGET_AARCH64_LINUX_ANDROID_LINKER="${ndk-bin}/aarch64-linux-android28-clang"
            export CARGO_TARGET_ARMV7_LINUX_ANDROIDEABI_LINKER="${ndk-bin}/armv7a-linux-androideabi28-clang"
            export CARGO_TARGET_I686_LINUX_ANDROID_LINKER="${ndk-bin}/i686-linux-android28-clang"
            export CARGO_TARGET_X86_64_LINUX_ANDROID_LINKER="${ndk-bin}/x86_64-linux-android28-clang"
            export AR_aarch64_linux_android="${ndk-bin}/llvm-ar"
            export AR_armv7_linux_androideabi="${ndk-bin}/llvm-ar"
            export AR_i686_linux_android="${ndk-bin}/llvm-ar"
            export AR_x86_64_linux_android="${ndk-bin}/llvm-ar"
          '';
          buildCmd = target: "${rustToolchain}/bin/cargo build --release --target \"${target}\"";
          copyCmd = target: ''
            DST="android/app/src/main/jniLibs/${abis.${target}}"
            SRC="../target/${target}/release/lib${libName}.so"
            mkdir -p "$DST"
            if [ ! -f "$DST/lib${libName}.so" ] || ! cmp -s "$SRC" "$DST/lib${libName}.so"; then
              cp "$SRC" "$DST/lib${libName}.so"
            fi
          '';
          pruneCmd = "find android/app/src/main/jniLibs -mindepth 1 -maxdepth 1 -type d ${builtins.concatStringsSep " " (map (t: "! -name \"${abis.${t}}\"") targets)} -exec rm -rf {} + 2>/dev/null || true";
          prepCmd = ''
            VSN="$(grep '^version' Cargo.toml | head -1 | sed -E 's/.*"([^"]+)".*/\1/')"
            for D in mdpi hdpi xhdpi xxhdpi xxxhdpi; do
              mkdir -p "android/app/src/main/res/mipmap-$D"
              SRC="${icons}/mipmap-$D.png"
              DST="android/app/src/main/res/mipmap-$D/ic_launcher.png"
              if [ ! -f "$DST" ] || ! cmp -s "$SRC" "$DST"; then
                cp "$SRC" "$DST"
              fi
            done
          '';
          bundleCmd = ''
            export ANDROID_HOME="${android-sdk}/libexec/android-sdk"
            export GRADLE_OPTS="-Djava.net.preferIPv4Stack=true -Dorg.gradle.project.android.aapt2FromMavenOverride=${android-sdk}/libexec/android-sdk/build-tools/36.0.0/aapt2"
            (cd android && gradle bundleRelease)
            OUT="android/app/build/outputs/bundle/release"
            cp "$OUT/app-release.aab" "$OUT/${app}-v$VSN.aab"
            echo "READY: ${app}/$OUT/${app}-v$VSN.aab"
          '';
        in
          pkgs.writeShellApplication {
            name = "release-aab-${app}";
            runtimeInputs = with pkgs; [coreutils gnugrep gnused gawk jdk findutils gradle];
            text = ''
              (
                cd "${app}"
                ${rustEnv}
                ${builtins.concatStringsSep "\n" (map buildCmd targets)}
                ${pruneCmd}
                ${builtins.concatStringsSep "\n" (map copyCmd targets)}
                ${prepCmd}
                ${bundleCmd}
                rm -f "$OUT/${app}-v$VSN-"*.aab
                echo "READY (universal, all ABIs): ${app}/$OUT/${app}-v$VSN.aab"
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
                printf '  {\n    "relation": ["delegate_permission/common.handle_all_urls"],\n    "target": {\n      "namespace": "android_app",\n      "package_name": "com.functora.%s",\n      "sha256_cert_fingerprints": ["%s", "6F:FA:9F:54:93:B0:CA:76:D5:0E:0A:5B:41:84:3A:7B:6E:F4:25:8B:AB:8C:23:13:98:76:D9:E8:AC:06:F6:2D"]\n    }\n  }\n' "$(echo "''${APPS[$i]}" | tr - _)" "$FP"
              done
              printf ']\n'
            } > "$DIR/.well-known/assetlinks.json"
            echo "Wrote $DIR/.well-known/assetlinks.json"
          '';
        };
        mkApk = app: aabDir: aabDrv:
          pkgs.writeShellApplication {
            name = "release-apk-${app}";
            runtimeInputs = with pkgs; [coreutils gnugrep gnused findutils jdk unzip bundletool];
            text = ''
              "${aabDrv}/bin/release-aab-${app}"
              VSN="$(cd "${app}" && grep '^version' Cargo.toml | head -1 | sed -E 's/.*"([^"]+)".*/\1/')"
              DIR="''${1:-${aabDir}}"

              export BUNDLETOOL_AAPT2_PATH="${android-sdk}/libexec/android-sdk/build-tools/36.0.0/aapt2"
              export JAVA_TOOL_OPTIONS="-Daapt2Path=$BUNDLETOOL_AAPT2_PATH"
              export BUNDLETOOL_AAPT2="$BUNDLETOOL_AAPT2_PATH"

              IFS= read -r -s -p "Keystore password: " KS_PASS
              echo

              FOUND=0
              for AAB in "$DIR"/*v"$VSN"*.aab; do
                [ -f "$AAB" ] || continue
                NAME="$(basename "$AAB" .aab)"
                [[ "$NAME" != *-signed ]] || continue

                STALE="$(cd "${app}" && find src Cargo.toml Cargo.lock -type f -newer "$AAB" 2>/dev/null | head -1 || true)"
                if [ -n "$STALE" ]; then
                  echo "AAB $AAB is older than $STALE (Rust sources changed). Run release-aab-${app} first."
                  exit 1
                fi

                FOUND=1
                SIG="$DIR/$NAME-signed.aab"
                APK="$DIR/$NAME.apk"
                TMP="$DIR/$NAME.apks"

                cp "$AAB" "$SIG"
                jarsigner \
                  -keystore "$HOME/keys/app-key.jks" \
                  -storepass "$KS_PASS" \
                  "$SIG" app-key

                bundletool build-apks \
                  --bundle="$AAB" \
                  --output="$TMP" \
                  --mode=universal \
                  --aapt2="$BUNDLETOOL_AAPT2_PATH" \
                  --ks="$HOME/keys/app-key.jks" \
                  --ks-pass=pass:"$KS_PASS" \
                  --ks-key-alias=app-key \
                  --overwrite

                unzip -p "$TMP" universal.apk > "$APK"
                rm -f "$TMP"
                echo "READY: $APK"
              done
              [ "$FOUND" -eq 1 ] || {
                echo "No v$VSN aab found in $DIR (current Cargo.toml version). Run release-aab-${app} first."
                exit 1
              }
            '';
          };
        shell = rec {
          releaseAabCryptonote = mkFunctoraEguiAab "cryptonote" "assets/favicon";
          releaseAabFunctoraEguiDemo = mkFunctoraEguiAab "functora-egui-demo" "assets/favicon";
          PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
          ANDROID_HOME = "${android-sdk}/libexec/android-sdk";
          ANDROID_SDK_ROOT = ANDROID_HOME;
          NDK_HOME = "${ANDROID_HOME}/ndk-bundle";
          ANDROID_NDK_HOME = NDK_HOME;
          GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${android-sdk}/libexec/android-sdk/build-tools/36.0.0/aapt2";
          CARGO_TARGET_X86_64_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/x86_64-linux-android28-clang";
          CARGO_TARGET_AARCH64_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android28-clang";
          CARGO_TARGET_ARMV7_LINUX_ANDROIDEABI_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi28-clang";
          CARGO_TARGET_I686_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/i686-linux-android28-clang";
          NDK_BIN = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin";
          CC_aarch64_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android28-clang";
          CXX_aarch64_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android28-clang++";
          CC_armv7_linux_androideabi = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi28-clang";
          CXX_armv7_linux_androideabi = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi28-clang++";
          CC_i686_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/i686-linux-android28-clang";
          CXX_i686_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/i686-linux-android28-clang++";
          CC_x86_64_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/x86_64-linux-android28-clang";
          CXX_x86_64_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/x86_64-linux-android28-clang++";
          CC_thumbv7neon_linux_androideabi = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi28-clang";
          CXX_thumbv7neon_linux_androideabi = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi28-clang++";
          AR_aarch64_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/llvm-ar";
          AR_armv7_linux_androideabi = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/llvm-ar";
          AR_i686_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/llvm-ar";
          AR_x86_64_linux_android = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/llvm-ar";
          AR_thumbv7neon_linux_androideabi = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/llvm-ar";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [
            libGL
            wayland
            libxkbcommon
            libx11
            libxcursor
            libxrandr
            libxi
            libxext
            gtk3
          ]);
          GSETTINGS_SCHEMA_DIR = "${gtk3-schemas}/glib-2.0/schemas";
          XDG_DATA_DIRS = "${pkgs.adwaita-icon-theme}/share:${pkgs.gtk3}/share:/usr/local/share:/usr/share";
          packages = with pkgs;
            [
              bacon
              rustToolchain
              rust-analyzer
              cargo-tarpaulin
              cargo-audit
              cargo-deny
              clippy
              wasmtime
              license-generator
              tailwindcss_4
              simple-http-server
              strace
              # web
              binaryen
              lessc
              clean-css-cli
              # linux
              pkg-config
              libGL
              wayland
              libxkbcommon
              libx11
              libxcursor
              libxrandr
              libxi
              libxext
              xvfb-run
              webkitgtk_4_1
              openssl
              xdotool
              adwaita-icon-theme
              libayatana-appindicator
              librsvg
              gtk3
              gdk-pixbuf
              cairo
              pango
              curl
              wget
              zlib
              file
              gcc
              # android
              aapt
              llvmPackages.lld
              llvmPackages.clang-unwrapped
              android-sdk
              glibc
              jdk
              gradle
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
              (mkFunctoraEguiWeb "cryptonote" "assets/favicon")
              (srFunctoraEguiWeb "cryptonote" "assets/favicon")
              (srFunctoraEguiDesktop "cryptonote")
              (mkApk "cryptonote" "./cryptonote/android/app/build/outputs/bundle/release" releaseAabCryptonote)
              (mkFunctoraEguiWeb "functora-egui-demo" "assets/favicon")
              (srFunctoraEguiWeb "functora-egui-demo" "assets/favicon")
              (srFunctoraEguiDesktop "functora-egui-demo")
              (mkApk "functora-egui-demo" "./functora-egui-demo/android/app/build/outputs/bundle/release" releaseAabFunctoraEguiDemo)
              (pkgs.writeShellApplication {
                name = "verify";
                text = ''
                  # Helper: check if crate has binary targets (is an application)
                  has_binary_targets() {
                    local crate_path="$1"
                    # Check for explicit binary targets in Cargo.toml
                    grep -q '\[\[bin\]\]' "$crate_path/Cargo.toml" 2>/dev/null || \
                    grep -q 'autobins = true' "$crate_path/Cargo.toml" 2>/dev/null || \
                    # Check for default binary entry point (src/main.rs)
                    [ -f "$crate_path/src/main.rs" ]
                  }

                  # Check if crate is an eframe web app (binary + eframe dep)
                  is_eframe_web_app() {
                    local crate_path="$1"
                    has_binary_targets "$crate_path" && \
                    grep -qE '^eframe\s*=' "$crate_path/Cargo.toml" 2>/dev/null
                  }

                  # Check if crate is an egui library (library + egui dep, for wasm32 lib checks)
                  is_egui_library() {
                    local crate_path="$1"
                    ! has_binary_targets "$crate_path" && \
                    grep -qE '^egui\s*=' "$crate_path/Cargo.toml" 2>/dev/null
                  }

                  verify_crate() {
                    local crate="$1"
                    shift
                    ${cargo}/bin/cargo fmt "$@" \
                      && ${cargo}/bin/cargo clippy --all-features --all-targets "$@" -- -D warnings \
                      && ${cargo}/bin/cargo test --all-features --all-targets "$@" \
                      && { if is_eframe_web_app "."; then for T in ${pkgs.lib.concatStringsSep " " mobile-targets}; do ${cargo}/bin/cargo clippy --target "$T" --all-features --all-targets "$@" -- -D warnings && echo "==> $crate [$T]: mobile clippy: All good!" || return 1; done; fi; } \
                      && { if is_eframe_web_app "." || is_egui_library "."; then ${cargo}/bin/cargo clippy --target wasm32-unknown-unknown --all-features --all-targets "$@" -- -D warnings && echo "==> $crate [wasm32-unknown-unknown]: clippy: All good!" || return 1; fi; }
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
                name = "audit";
                text = ''
                  audit_crate() {
                    local crate="$1"
                    shift
                    ${pkgs.cargo-audit}/bin/cargo-audit audit --file Cargo.lock "$@" \
                      && ${pkgs.cargo-deny}/bin/cargo-deny check "$@" \
                      && echo "==> $crate: no known vulnerabilities"
                  }
                  if [ -f Cargo.toml ] && [ -f Cargo.lock ]; then
                    audit_crate "$(basename "$PWD")" "$@" && echo "==> All good!"
                  else
                    FOUND=0
                    for P in */; do
                      P="''${P%/}"
                      [ -f "$P/Cargo.lock" ] || continue
                      FOUND=1
                      (
                        cd "$P"
                        audit_crate "$P" "$@" && echo "==> $P: All good!"
                      ) || {
                        echo "==> $P: FAILED"
                        exit 1
                      }
                    done
                    if [ "$FOUND" -eq 0 ]; then
                      echo "==> No crate with a Cargo.lock found in this directory!"
                      exit 1
                    fi
                    echo "==> All crates audited!"
                  fi
                '';
              })
              (pkgs.writeShellApplication {
                name = "tunnel-8000";
                text = ''
                  ${pkgs.cloudflared}/bin/cloudflared tunnel --protocol http2 --edge-ip-version 4 --url http://localhost:8000
                '';
              })
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              (mkFunctoraEguiDesktop "cryptonote")
              (mkFunctoraEguiDesktop "functora-egui-demo")
            ]
            ++ [releaseAabCryptonote]
            ++ [releaseAabFunctoraEguiDemo];
        };
      in {
        devShells.default = pkgs.mkShell shell;
        packages = desktopPackages // {
          default = desktopPackages.cryptonote;
        } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          cryptonote-bundle = desktopBundles.cryptonote;
          functora-egui-demo-bundle = desktopBundles.functora-egui-demo;
        };
        apps = desktopApps;
        bundlers = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          default = mkNixBundle;
          toArx = mkNixBundle;
        };
        checks = {
          cryptonote = desktopPackages.cryptonote;
          functora-egui-demo = desktopPackages.functora-egui-demo;
        } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          cryptonote-bundle = desktopBundleCheck "cryptonote" desktopBundles.cryptonote desktopBundleRuntimes.cryptonote;
          functora-egui-demo-bundle = desktopBundleCheck "functora-egui-demo" desktopBundles.functora-egui-demo desktopBundleRuntimes.functora-egui-demo;
        };
      }
    );
}
