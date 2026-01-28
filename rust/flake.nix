{
  description = "Rust Dev Shell";

  inputs = {
    unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    unstable,
    rust-overlay,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import unstable {
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
        wasm-bindgen-cli-0_2_106 = with pkgs;
          rustPlatform.buildRustPackage rec {
            pname = "wasm-bindgen-cli";
            version = "0.2.106";
            src = pkgs.fetchCrate {
              pname = "wasm-bindgen-cli";
              version = "0.2.106";
              sha256 = "sha256-M6WuGl7EruNopHZbqBpucu4RWz44/MSdv6f0zkYw+44=";
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
          platformVersions = ["33" "34"];
          buildToolsVersions = ["33.0.2" "34.0.0"];
          abiVersions = ["armeabi-v7a" "arm64-v8a" "x86" "x86_64"];
          systemImageTypes = ["default" "google_apis_playstore"];
          includeNDK = true;
        };
        android-sdk =
          (pkgs.androidenv.composeAndroidPackages android-sdk-args).androidsdk;
        mkAab = app: let
          mkCmd = target: ''
            dx bundle --release --android --debug-symbols=false --target "${target}"
            echo "Aab ${app} release success for ${target}!"
          '';
        in
          (
            map (
              target:
                pkgs.writeShellApplication {
                  name = "release-aab-${app}-${target}";
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
            runtimeInputs = [pkgs.coreutils pkgs.gnugrep pkgs.gnused];
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
                cp -R ./target/dx/cryptonote/release/web/public/* "$REL"
                echo "<!doctype html><html><head><meta http-equiv=\"Refresh\" content=\"0; url=$VSN\"></head><body></body></html>" > ../../apps/${app}/index.html
                echo "$REL web release success!"
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

            magick "$INPUT" -resize 48x48   "$DIR/mipmap-mdpi.png"
            magick "$INPUT" -resize 72x72   "$DIR/mipmap-hdpi.png"
            magick "$INPUT" -resize 96x96   "$DIR/mipmap-xhdpi.png"
            magick "$INPUT" -resize 144x144 "$DIR/mipmap-xxhdpi.png"
            magick "$INPUT" -resize 192x192 "$DIR/mipmap-xxxhdpi.png"
            magick "$INPUT" -resize 432x432 "$DIR/android-foreground.png"
            magick -size 432x432 xc:#1E1E2E "$DIR/android-background.png"
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

              export BUNDLETOOL_AAPT2_PATH="${android-sdk}/libexec/android-sdk/build-tools/33.0.2/aapt2";
              export JAVA_TOOL_OPTIONS="-Daapt2Path=$BUNDLETOOL_AAPT2_PATH"
              export BUNDLETOOL_AAPT2="$BUNDLETOOL_AAPT2_PATH"

              for AAB in "$DIR"/*.aab; do
                [ -f "$AAB" ] || continue

                NAME=$(${pkgs.coreutils}/bin/basename "$AAB" .aab)
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
          GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${android-sdk}/libexec/android-sdk/build-tools/33.0.2/aapt2";
          CARGO_TARGET_X86_64_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/x86_64-linux-android28-clang";
          CARGO_TARGET_AARCH64_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android28-clang";
          CARGO_TARGET_ARMV7_LINUX_ANDROIDEABI_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi28-clang";
          CARGO_TARGET_I686_LINUX_ANDROID_LINKER = "${android-sdk}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/bin/i686-linux-android28-clang";
          packages = with pkgs;
            [
              alejandra
              bacon
              rustToolchain
              rust-analyzer
              cargo-tarpaulin
              clippy
              wasmtime
              vimb
              license-generator
              dioxus-cli
              tailwindcss_4
              simple-http-server
              strace
              # web
              binaryen
              wasm-bindgen-cli-0_2_106
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
              llvmPackages.lld
              llvmPackages.clang-unwrapped
              android-sdk
              glibc
              jdk
              android-icons
              android-keygen
              # fonts
              noto-fonts
              noto-fonts-cjk-sans
              noto-fonts-color-emoji
              liberation_ttf
              dejavu_fonts
              # apps
              (mkWeb "cryptonote")
              (mkApk "cryptonote")
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
          pkgs = import unstable {
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
        devShells.unfree = pkgs.mkShell (shell
          // {
            packages =
              shell.packages
              ++ [
                pkgs.qutebrowser
                unstable.legacyPackages.${system}.antigravity
                (pkgs.writeShellApplication {
                  name = "cursor";
                  text = ''
                    ${
                      unstable.legacyPackages.${system}.cursor-cli
                    }/bin/cursor-agent -f --model auto "$@"
                  '';
                })
              ];
          });
        packages = rec {
          rustell = mkRustPkg "rustell";
          rustell-nvim = mkRustellNvim "${rustell}/bin/rustell";
          rustell-wasm = mkRustPkgWasm "rustell";
          rustell-wasm-nvim = mkRustellNvim "${
            pkgs.wasmtime
          }/bin/wasmtime ${rustell-wasm}/lib/rustell.wasm";
          default = self.packages.${system}.rustell;
        };
      }
    );
}
