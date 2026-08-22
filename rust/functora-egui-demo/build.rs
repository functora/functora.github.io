#![allow(clippy::unwrap_used, clippy::expect_used)]
use askama::Template;

fn main() {
    println!("cargo:rerun-if-changed=Cargo.toml");
    println!("cargo:rerun-if-changed=../functora-egui/assets/web/egui.js");
    println!("cargo:rerun-if-changed=../functora-egui/templates/web/index.html");
    println!("cargo:rerun-if-changed=../functora-egui/templates/web/manifest.json");
    println!("cargo:rerun-if-changed=../functora-egui/templates/android/build.gradle");
    println!("cargo:rerun-if-changed=../functora-egui/templates/android/settings.gradle");
    println!("cargo:rerun-if-changed=../functora-egui/templates/android/gradle.properties");
    println!("cargo:rerun-if-changed=../functora-egui/templates/android/app/build.gradle");
    println!(
        "cargo:rerun-if-changed=../functora-egui/templates/android/app/src/main/AndroidManifest.xml"
    );
    println!(
        "cargo:rerun-if-changed=../functora-egui/templates/android/app/src/main/java/MainActivity.java"
    );
    println!(
        "cargo:rerun-if-changed=../functora-egui/templates/android/app/src/main/res/values/styles.xml"
    );

    let android_cfg = functora_egui::android::config::load_android_config("Cargo.toml");
    let web_cfg = functora_egui::web::config::load_config("Cargo.toml");

    let settings = functora_egui::android::templates::SettingsGradle {
        app_name: &android_cfg.app_name,
    }
    .render()
    .expect("askama settings");
    let app_build = functora_egui::android::templates::AppBuildGradle {
        namespace: &android_cfg.namespace,
        application_id: &android_cfg.application_id,
        version_code: android_cfg.version_code,
        version_name: &android_cfg.version_name,
    }
    .render()
    .expect("askama app build");
    let manifest = functora_egui::android::templates::ManifestXml {
        label: &android_cfg.label,
        activity_fqn: &android_cfg.activity_fqn,
        lib_name: &android_cfg.lib_name,
        host: &android_cfg.host,
        path_prefix: &android_cfg.path_prefix,
        extra_intent_filters: &android_cfg.extra_intent_filters,
    }
    .render()
    .expect("askama manifest");
    let java = functora_egui::android::templates::MainActivity {
        package: &android_cfg.namespace,
        lib_name: &android_cfg.lib_name,
    }
    .render()
    .expect("askama java");
    let root_build = functora_egui::android::templates::RootBuildGradle
        .render()
        .expect("askama root build");
    let gradle_props = functora_egui::android::templates::GradleProperties
        .render()
        .expect("askama gradle props");
    let styles = functora_egui::android::templates::Styles
        .render()
        .expect("askama styles");

    std::fs::create_dir_all("android").unwrap();
    std::fs::write("android/build.gradle", root_build).unwrap();
    std::fs::write("android/settings.gradle", settings).unwrap();
    std::fs::write("android/gradle.properties", gradle_props).unwrap();
    std::fs::create_dir_all("android/app/src/main/res/values").unwrap();
    std::fs::write("android/app/src/main/res/values/styles.xml", styles).unwrap();
    std::fs::create_dir_all("android/app").unwrap();
    std::fs::write("android/app/build.gradle", app_build).unwrap();
    std::fs::create_dir_all("android/app/src/main").unwrap();
    std::fs::write("android/app/src/main/AndroidManifest.xml", manifest).unwrap();
    drop(std::fs::remove_dir_all("android/app/src/main/java"));
    let java_dir = format!(
        "android/app/src/main/java/{}",
        android_cfg.namespace.replace('.', "/")
    );
    std::fs::create_dir_all(&java_dir).unwrap();
    std::fs::write(format!("{java_dir}/MainActivity.java"), java).unwrap();

    if std::env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default() != "wasm32" {
        return;
    }

    let index = functora_egui::web::templates::IndexHtml {
        title: &web_cfg.title,
        theme_color: &web_cfg.theme_color,
        pkg_js: &web_cfg.pkg_js,
        vsn: &web_cfg.vsn,
    }
    .render()
    .expect("askama index");
    let manifest_web = functora_egui::web::templates::ManifestJson {
        title: &web_cfg.title,
        short_name: &web_cfg.short_name,
        theme_color: &web_cfg.theme_color,
    }
    .render()
    .expect("askama manifest web");

    std::fs::create_dir_all("assets").unwrap();
    std::fs::write("assets/index.html", index).unwrap();
    std::fs::write("assets/manifest.webmanifest", manifest_web).unwrap();
    drop(std::fs::copy(
        "../functora-egui/assets/web/egui.js",
        "assets/egui.js",
    ));
}
