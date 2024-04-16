let
  # functora = ../..;
  functora = fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/5d7ba2e134b7fac23e1a4f4d707a6fbe6ad87192.tar.gz";
    sha256 = "0gzc9vr691n1ss4ghxlyw6xys1rpcfysj6pmy2h2jvyv0m9frykv";
  };
  functora-miso = import "${functora}/ghcjs/miso/default.nix" {
    overlays = import ./overlays.nix {inherit functora;};
  };
  pkgs = functora-miso.pkgs;
  doCheck = pkgs.haskell.lib.doCheck;
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
  source = pkgs.nix-gitignore.gitignoreSourcePure ./.gitignore ./.;
  dev = doCheck (pkgs.haskell.packages.ghc865.callCabal2nix "app" source {
    miso = functora-miso.miso-jsaddle;
  });
  app = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" source {};
  vsn = app.passthru.version;
  repo = builtins.toString ./.;
  app-serve-latest = functora-pkgs.writeShellApplication rec {
    name = "app-serve-latest";
    text = ''
      ${functora-pkgs.simple-http-server}/bin/simple-http-server \
        -p 3000 ${repo}/dist/latest
    '';
  };
  app-release-latest = functora-pkgs.writeShellApplication rec {
    name = "app-release-latest";
    text = ''
      (
        cd ${repo}
        nix-build -A releaseDer
        rm -rf ./dist/latest
        mkdir -p ./dist/latest
        ${safeCopy} ./result/* ./dist/latest
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
  releaseDer = functora-pkgs.stdenv.mkDerivation {
    name = "releaseDer";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/static
      cp ${./static}/*.png $out/static/
      cp ${./static}/*.woff2 $out/static/
      cp ${./static}/*.webmanifest $out/
      cp ${./static}/*.ico $out/
      ${functora-pkgs.html-minifier}/bin/html-minifier \
        --minify-js \
        --minify-css \
        -o $out/license.html \
        ${licenseDer}/license.html
      ${functora-pkgs.html-minifier}/bin/html-minifier \
        --minify-js \
        --minify-css \
        -o $out/privacy.html \
        ${privacyDer}/privacy.html
      ${functora-pkgs.html-minifier}/bin/html-minifier \
        --minify-js \
        --minify-css \
        -o $out/index.html \
        ${./static}/index.html
      ${functora-pkgs.clean-css-cli}/bin/cleancss \
        -O2 \
        --source-map \
        -o $out/static/all.css \
        ${./static}/material-components-web.min.css \
        ${./static}/material-icons.css \
        ${./static}/app.css
      ${functora-pkgs.closurecompiler}/bin/closure-compiler \
        --jscomp_off=checkVars \
        --compilation_level ADVANCED_OPTIMIZATIONS \
        --externs ${app}/bin/app.jsexe/all.js.externs \
        --externs ${./static}/material-components-web.min.js \
        --output_wrapper "%output%//# sourceMappingURL=all.js.map" \
        --create_source_map $out/all.js.map \
        --js ${app}/bin/app.jsexe/all.js \
        --js_output_file $out/all.js
    '';
  };
  releaseStableDer = functora-pkgs.stdenv.mkDerivation {
    name = "releaseStableDer";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/${vsn}
      cp -R ${releaseDer}/* $out/${vsn}
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
    src = ./LICENSE;
    dontUnpack = true;
    buildPhase = ''
      echo "# [Back](index.html)" > ./index-link.md
      ${functora-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --from markdown \
        --metadata title="LICENSE" \
        $src ./index-link.md > license.html
    '';
    installPhase = ''
      mkdir -p $out
      cp ./license.html $out/license.html
    '';
  };
  privacyDer = functora-pkgs.stdenv.mkDerivation {
    name = "privacyDer";
    src = ./privacy.md;
    dontUnpack = true;
    buildPhase = ''
      echo "# [Back](index.html)" > ./index-link.md
      ${functora-pkgs.pandoc}/bin/pandoc \
        --standalone \
        --from markdown \
        --metadata title="PRIVACY POLICY" \
        $src ./index-link.md > privacy.html
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
