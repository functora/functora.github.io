with (import ./default.nix); let
  pkgs = android-pkgs;
  repo = toString ./.;
  android-sdk =
    (pkgs.androidenv.composeAndroidPackages android-sdk-args).androidsdk;
  app-keygen-android = pkgs.writeShellApplication {
    name = "app-keygen-android";
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
  app-sign-apk = pkgs.writeShellApplication {
    name = "app-sign-apk";
    text = ''
      ${pkgs.apksigner}/bin/apksigner sign \
        --ks ~/keys/app-key.jks \
        --out ${repo}/android/app.apk \
        ${repo}/android/app/build/outputs/apk/release/app-release-unsigned.apk
    '';
  };
  app-sign-aab = pkgs.writeShellApplication {
    name = "app-sign-aab";
    text = ''
      ${pkgs.zulu}/bin/jarsigner \
        -verbose \
        -keystore ~/keys/app-key.jks \
        -signedjar ${repo}/android/app.aab \
        ${repo}/android/app/build/outputs/bundle/release/app-release.aab \
        app-key
    '';
  };
  app-prepare-android = pkgs.writeShellApplication rec {
    name = "app-prepare-android";
    text = ''
      (
        cd ${repo}
        ${pkgs.nodejs}/bin/npm i
        ${pkgs.nodejs}/bin/npm run build
        ${pkgs.nodejs}/bin/npx cap add android || true
        ${pkgs.nodejs}/bin/npx cap sync
        cp ${repo}/static/android-chrome-512x512.png ${repo}/static/logo.png
        ${pkgs.nodejs}/bin/npx @capacitor/assets generate \
          --android --assetPath static
        ${pkgs.nodejs}/bin/npx trapeze run trapeze.yaml -y \
          --android-project android
      )
    '';
  };
  app-release-apk = pkgs.writeShellApplication rec {
    name = "app-release-apk";
    text = ''
      (
        cd ${repo}
        ${app-prepare-android}/bin/app-prepare-android
        cd ./android
        ./gradlew assembleRelease
        ${app-keygen-android}/bin/app-keygen-android
        rm ${repo}/android/app.apk || true
        ${app-sign-apk}/bin/app-sign-apk
        ls -la ${repo}/android/app.apk
      )
    '';
  };
  app-release-aab = pkgs.writeShellApplication rec {
    name = "app-release-aab";
    text = ''
      (
        cd ${repo}
        ${app-prepare-android}/bin/app-prepare-android
        cd ./android
        ./gradlew bundleRelease
        ${app-keygen-android}/bin/app-keygen-android
        rm ${repo}/android/app.aab || true
        ${app-sign-aab}/bin/app-sign-aab
        ls -la ${repo}/android/app.aab
      )
    '';
  };
in
  pkgs.mkShell rec {
    buildInputs = with pkgs; [
      alejandra
      android-sdk
      glibc
      jdk
      zulu
      nodejs
      app-release-apk
      app-release-aab
    ];
    GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${android-sdk}/libexec/android-sdk/build-tools/34.0.0/aapt2";
    ANDROID_SDK_ROOT = "${android-sdk}/libexec/android-sdk";
    ANDROID_NDK_ROOT = "${ANDROID_SDK_ROOT}/ndk-bundle";
  }
