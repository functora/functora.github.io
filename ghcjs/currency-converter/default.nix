let
  # functora = ../..;
  functora = fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/e61ec6fe82cf03a1a5355e4765b04168296d6b54.tar.gz";
    sha256 = "0i3pm9zskm18z74bkyy4wfycvfxbxkc9djw1c9iarl9j79xb6p1c";
  };
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
  vsn = app.passthru.version;
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
    text = ''
      (
        cd ${repo}
        nix-build -A releaseStableDer
        rm -rf ./dist/${vsn}
        mkdir -p ./dist/${vsn}
        ${safeCopy} ./result/${vsn}/* ./dist/${vsn}
        ${forceCopy} ./result/${vsn}/favicon.ico ./dist/favicon.ico
        ${forceCopy} ./result/index.html ./dist/index.html
      )
    '';
  };
  app-release-license = functora-pkgs.writeShellApplication rec {
    name = "app-release-license";
    text = ''
      ${functora-pkgs.lice}/bin/lice \
        -l txt -y 2024 -o Functora mit > ${repo}/LICENSE
    '';
  };
  releaseDer = app.overrideAttrs (_: rec {
    postInstall = ''
      mkdir -p $out/bin/app.jsexe/static
      cp -R ${./static}/* $out/bin/app.jsexe/static
      cp ${./static}/favicon.ico $out/bin/app.jsexe/favicon.ico
      cp ${readmeDer}/readme.html $out/bin/app.jsexe/
      cp ${licenseDer}/license.html $out/bin/app.jsexe/
      cp ${privacyDer}/privacy.html $out/bin/app.jsexe/
    '';
  });
  releaseStableDer = functora-pkgs.stdenv.mkDerivation {
    name = "releaseStableDer";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/${vsn}
      cp -R ${releaseDer}/bin/app.jsexe/* $out/${vsn}
      echo '<!doctype html><html><head><meta http-equiv="Refresh" content="0; url=${vsn}/index.html"></head><body></body></html>' > $out/index.html
      echo "Version ${vsn} has been released!"
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
  licenseDer = functora-pkgs.stdenv.mkDerivation {
    name = "licenseDer";
    src = ./.;
    dontUnpack = true;
    buildPhase = ''
      echo "# [Back](index.html)" > ./index-link.md
      ${functora-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --from markdown \
        --metadata title="LICENSE" \
        $src/LICENSE ./index-link.md > license.html
    '';
    installPhase = ''
      mkdir -p $out
      cp ./license.html $out/license.html
    '';
  };
  privacyDer = functora-pkgs.stdenv.mkDerivation {
    name = "privacyDer";
    src = ./.;
    dontUnpack = true;
    buildPhase = ''
      echo "# [Back](index.html)" > ./index-link.md
      ${functora-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --from markdown \
        --metadata title="PRIVACY POLICY" \
        $src/privacy.md ./index-link.md > privacy.html
    '';
    installPhase = ''
      mkdir -p $out
      cp ./privacy.html $out/privacy.html
    '';
  };
  app-publish-stable = functora-pkgs.writeShellApplication rec {
    name = "app-publish-stable";
    text = ''
      (
        ${app-release-stable}/bin/app-release-stable
        if [ -d "${repo}/../../pub/functora-hakyll/apps/currency-converter/${vsn}" ]
        then
          echo "Version ${vsn} does already exist!"
          exit 1
        else
          mkdir -p ${repo}/../../pub/functora-hakyll/apps/currency-converter/${vsn}
        fi
        ${safeCopy} ${repo}/dist/${vsn}/* \
          ${repo}/../../pub/functora-hakyll/apps/currency-converter/${vsn}
        ${forceCopy} ${repo}/dist/index.html \
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
