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
    println!(
        "cargo:rerun-if-changed=../functora-egui/templates/android/app/src/main/java/Waker.java"
    );
    println!("cargo:rerun-if-changed=assets/favicon/mipmap-mdpi.png");
    println!("cargo:rerun-if-changed=assets/favicon/mipmap-hdpi.png");
    println!("cargo:rerun-if-changed=assets/favicon/mipmap-xhdpi.png");
    println!("cargo:rerun-if-changed=assets/favicon/mipmap-xxhdpi.png");
    println!("cargo:rerun-if-changed=assets/favicon/mipmap-xxxhdpi.png");
    println!("cargo:rerun-if-changed=../functora-egui/templates/desktop/app.desktop");
    println!("cargo:rerun-if-changed=../functora-egui/templates/desktop/metainfo.xml");
    println!("cargo:rerun-if-changed=assets/icon.png");

    let android_cfg = functora_egui::android::config::load_android_config("Cargo.toml");
    let web_cfg = functora_egui::web::config::load_config("Cargo.toml");
    let desktop_cfg = functora_egui::desktop::config::load_desktop_config("Cargo.toml");
    println!("cargo:rustc-env=DEMO_DESKTOP_APP_ID={}", desktop_cfg.app_id);

    println!("cargo:rustc-env=DEMO_WEB_TITLE={}", web_cfg.title);
    println!("cargo:rustc-env=DEMO_WEB_SHORT_NAME={}", web_cfg.short_name);
    println!(
        "cargo:rustc-env=DEMO_WEB_THEME_COLOR={}",
        web_cfg.theme_color
    );

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
        camera: android_cfg.camera,
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
    let waker = functora_egui::android::templates::Waker
        .render()
        .expect("askama waker");

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
    std::fs::create_dir_all("android/app/src/main/java/com/functora").unwrap();
    std::fs::write("android/app/src/main/java/com/functora/Waker.java", waker).unwrap();
    if android_cfg.camera {
        // Stale helper from earlier builds must not linger in the gradle src tree.
        if let Err(e) = std::fs::remove_dir_all("android/app/src/main/java/functora")
            && e.kind() != std::io::ErrorKind::NotFound
        {
            eprintln!("failed to remove stale camera helper: {e}");
        }
    }
    let _ = functora_egui::android::icons::copy_launcher_icons(
        std::path::Path::new("android"),
        std::path::Path::new("assets/favicon"),
    )
    .expect("copy launcher icons");

    let desktop_entry = functora_egui::desktop::templates::LinuxDesktop {
        title: &desktop_cfg.title,
        comment: &desktop_cfg.comment,
        exec_name: &desktop_cfg.exec_name,
        icon_name: &desktop_cfg.icon_name,
        categories: &desktop_cfg.categories,
        mime_types: &desktop_cfg.mime_types,
        schemes: &desktop_cfg.schemes,
    }
    .render()
    .expect("askama desktop entry");
    let metainfo = functora_egui::desktop::templates::Metainfo {
        app_id: &desktop_cfg.app_id,
        title: &desktop_cfg.title,
        comment: &desktop_cfg.comment,
        version: &desktop_cfg.version,
    }
    .render()
    .expect("askama metainfo");
    std::fs::create_dir_all("desktop/linux").unwrap();
    std::fs::write(
        format!("desktop/linux/{}.desktop", desktop_cfg.app_id),
        desktop_entry,
    )
    .unwrap();
    std::fs::create_dir_all("desktop/metainfo").unwrap();
    std::fs::write(
        format!("desktop/metainfo/{}.metainfo.xml", desktop_cfg.app_id),
        metainfo,
    )
    .unwrap();
    let _ = functora_egui::desktop::icons::copy_desktop_icons(
        std::path::Path::new("desktop"),
        std::path::Path::new("assets"),
        &desktop_cfg.app_id,
    )
    .expect("copy desktop icons");

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
