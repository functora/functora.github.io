let
  functora = ../..;
  functora-miso = import "${functora}/ghcjs/miso/default.nix" {
    overlays = import ./overlays.nix {inherit functora;};
  };
  pkgs = functora-miso.pkgs;
  functora-pkgs = import "${functora}/nix/nixpkgs.nix";
  safeCopy = "cp -RL --no-preserve=mode,ownership";
  forceCopy = "cp -RLf --no-preserve=mode,ownership";
  android-pkgs = import "${functora}/nix/android.nix";
  android-sdk-args = {
    platformVersions = ["33"];
    buildToolsVersions = ["30.0.3" "34.0.0"];
    abiVersions = ["armeabi-v7a" "arm64-v8a" "x86" "x86_64"];
    systemImageTypes = ["default" "google_apis_playstore"];
  };
in rec {
  inherit pkgs functora android-pkgs android-sdk-args;
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {
    miso = functora-miso.miso-jsaddle;
  };
  app = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
  repo = builtins.toString ./.;
  app-release-latest = functora-pkgs.writeShellApplication rec {
    name = "app-release-latest";
    text = ''
      (
        cd ${repo}
        nix-build -A releaseDer
        rm -rf ./dist/latest
        mkdir -p ./dist/latest
        ${safeCopy} ./result/bin/app.jsexe/* ./dist/latest
      )
    '';
  };
  app-release-stable = functora-pkgs.writeShellApplication rec {
    name = "app-release-stable";
    text = let
      vsn = app.passthru.version;
    in ''
      (
        ${app-release-latest}/bin/app-release-latest
        cd ${repo}
        nix-build -A releaseStableDer
        mkdir -p ./dist/${vsn}
        ${safeCopy} ./result/${vsn}/* ./dist/${vsn}
        ${forceCopy} ./result/${vsn}/favicon.ico ./dist/favicon.ico
        ${forceCopy} ./result/index.html ./dist/index.html
      )
    '';
  };
  app-release-readme = functora-pkgs.writeShellApplication rec {
    name = "app-release-readme";
    text = ''
      ${functora-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --metadata title="Functora" \
        ${repo}/README.md > ${repo}/static/readme.html
    '';
  };
  releaseDer = app.overrideAttrs (_: rec {
    postInstall = ''
      mkdir -p $out/bin/app.jsexe/static
      cp -R ${./static}/* $out/bin/app.jsexe/static
      cp ${./static}/favicon.ico $out/bin/app.jsexe/favicon.ico
      rm $out/bin/app.jsexe/static/readme.html || true
      cp ${readmeDer}/readme.html $out/bin/app.jsexe/static/
    '';
  });
  releaseStableDer = functora-pkgs.stdenv.mkDerivation {
    name = "releaseStableDer";
    src = ./dist;
    dontBuild = true;
    installPhase = let
      vsn = app.passthru.version;
    in ''
      mkdir -p $out
      cp -R ./* $out || true
      if [ -d "$out/${vsn}" ]; then
        echo "Version ${vsn} does already exit!"
        exit 1
      else
        mkdir -p $out/${vsn}
        cp -R ${releaseDer}/bin/app.jsexe/* $out/${vsn}
        echo '<!doctype html><html><head><meta http-equiv="Refresh" content="0; url=${vsn}/index.html"></head><body></body></html>' > $out/index.html
        echo "Version ${vsn} has been released!"
      fi
    '';
  };
  readmeDer = functora-pkgs.stdenv.mkDerivation {
    name = "readmeDer";
    src = ./README.md;
    dontUnpack = true;
    buildPhase = ''
      ${functora-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --metadata title="Functora" \
        $src > readme.html
    '';
    installPhase = ''
      mkdir -p $out
      cp ./readme.html $out/readme.html
    '';
  };
  app-publish = functora-pkgs.writeShellApplication rec {
    name = "app-publish";
    text = ''
      (
        ${app-release-latest}/bin/app-release-latest
        rm -rf ${repo}/../../pub/functora-hakyll/apps/currency-converter
        mkdir -p ${repo}/../../pub/functora-hakyll/apps/currency-converter
        ${safeCopy} ${repo}/dist/* \
          ${repo}/../../pub/functora-hakyll/apps/currency-converter
      )
    '';
  };
  #
  # NOTE : broken atm https://github.com/NixOS/nixpkgs/issues/187853
  #
  # emulateAndroidDer = android-pkgs.androidenv.emulateApp {
  #   name = "emulateAndroidDer";
  #   platformVersion = "33";
  #   abiVersion = "armeabi-v7a";
  #   systemImageType = "default";
  #   app =
  #     ./android/app/build/outputs/apk/release/app-release-unsigned.apk;
  #   package = "App";
  #   activity = "MainActivity";
  #   sdkExtraArgs = android-sdk-args;
  # };
}
