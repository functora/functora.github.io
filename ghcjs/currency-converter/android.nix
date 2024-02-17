with (import ./default.nix); let
  pkgs = android-pkgs;
  repo = toString ./.;
  android-sdk =
    (pkgs.androidenv.composeAndroidPackages android-sdk-args).androidsdk;
  app-keygen-android = pkgs.writeShellApplication {
    name = "app-keygen-android";
    text = ''
      if [ ! -f "${repo}/android/keys/app-key.jks" ]; then
        mkdir -p ${repo}/android/keys
        ${pkgs.zulu}/bin/keytool -genkey -v \
          -keystore ${repo}/android/keys/app-key.jks \
          -keyalg RSA \
          -keysize 2048 \
          -validity 10000 \
          -alias app-key
      fi
    '';
  };
  app-sign-android = pkgs.writeShellApplication {
    name = "app-sign-android";
    text = ''
      ${pkgs.apksigner}/bin/apksigner sign \
        --ks ${repo}/android/keys/app-key.jks \
        --out ${repo}/android/app.apk \
        ${repo}/android/app/build/outputs/apk/release/app-release-unsigned.apk
    '';
  };
  app-release-android = pkgs.writeShellApplication rec {
    name = "app-release-android";
    text = ''
      (
        cd ${repo}
        ${app-release-latest}/bin/app-release-latest
        ${pkgs.nodejs}/bin/npm i
        ${pkgs.nodejs}/bin/npx cap add android || true
        ${pkgs.nodejs}/bin/npx cap sync
        cp ${repo}/static/android-chrome-512x512.png ${repo}/static/logo.png
        ${pkgs.nodejs}/bin/npx @capacitor/assets generate \
          --android --assetPath "${repo}/static"
        cd ./android
        ./gradlew assembleRelease
        ${app-keygen-android}/bin/app-keygen-android
        rm ${repo}/android/app.apk || true
        ${app-sign-android}/bin/app-sign-android
        ls -la ${repo}/android/app.apk
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
      nodejs
      app-release-android
    ];
    GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${android-sdk}/libexec/android-sdk/build-tools/34.0.0/aapt2";
    ANDROID_SDK_ROOT = "${android-sdk}/libexec/android-sdk";
    ANDROID_NDK_ROOT = "${ANDROID_SDK_ROOT}/ndk-bundle";
  }
