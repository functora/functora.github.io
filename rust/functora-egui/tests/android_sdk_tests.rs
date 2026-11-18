const APP_BUILD_GRADLE: &str = include_str!("../templates/android/app/build.gradle");
const ROOT_BUILD_GRADLE: &str = include_str!("../templates/android/build.gradle");

#[test]
fn android_app_targets_api_36() {
    assert!(
        APP_BUILD_GRADLE.contains("compileSdk = 36"),
        "Android apps must compile against API 36"
    );
    assert!(
        APP_BUILD_GRADLE.contains("targetSdk = 36"),
        "Android apps must target API 36"
    );
}

#[test]
fn android_gradle_plugin_supports_api_36() {
    assert!(
        ROOT_BUILD_GRADLE.contains("id \"com.android.application\" version \"8.9.1\" apply false"),
        "Android Gradle plugin 8.9.1 or newer is required for API 36"
    );
}
