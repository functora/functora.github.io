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
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          targets = [
            # web
            "wasm32-unknown-unknown"
            # mobile
            "aarch64-linux-android"
            "arm-linux-androideabi"
            "armv7-linux-androideabi"
            "i686-linux-android"
            "thumbv7neon-linux-androideabi"
            "x86_64-linux-android"
          ];
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
          packages = with pkgs; [
            alejandra
            bacon
            rustToolchain
            rust-analyzer
            cargo-tarpaulin
            clippy
            wasmtime
            license-generator
            dioxus-cli
            tailwindcss_4
            simple-http-server
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
            # fonts
            noto-fonts
            noto-fonts-cjk-sans
            noto-fonts-color-emoji
            liberation_ttf
            dejavu_fonts
          ];
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
