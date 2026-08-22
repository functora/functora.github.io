use askama::Template;

#[derive(Template)]
#[template(path = "android/settings.gradle", escape = "none", ext = "txt")]
pub struct SettingsGradle<'a> {
    pub app_name: &'a str,
}

#[derive(Template)]
#[template(path = "android/app/build.gradle", escape = "none", ext = "txt")]
pub struct AppBuildGradle<'a> {
    pub namespace: &'a str,
    pub application_id: &'a str,
    pub version_code: u32,
    pub version_name: &'a str,
}

#[derive(Template)]
#[template(
    path = "android/app/src/main/AndroidManifest.xml",
    escape = "none",
    ext = "xml"
)]
pub struct ManifestXml<'a> {
    pub label: &'a str,
    pub activity_fqn: &'a str,
    pub lib_name: &'a str,
    pub host: &'a str,
    pub path_prefix: &'a str,
    pub extra_intent_filters: &'a str,
}

#[derive(Template)]
#[template(
    path = "android/app/src/main/java/MainActivity.java",
    escape = "none",
    ext = "java"
)]
pub struct MainActivity<'a> {
    pub package: &'a str,
    pub lib_name: &'a str,
}

#[derive(Template)]
#[template(path = "android/build.gradle", escape = "none", ext = "txt")]
pub struct RootBuildGradle;

#[derive(Template)]
#[template(path = "android/gradle.properties", escape = "none", ext = "txt")]
pub struct GradleProperties;

#[derive(Template)]
#[template(
    path = "android/app/src/main/res/values/styles.xml",
    escape = "none",
    ext = "xml"
)]
pub struct Styles;
