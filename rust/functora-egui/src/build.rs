use crate::android::config::{AndroidConfig, load_android_config};
use crate::android::icons::copy_launcher_icons;
use crate::desktop::config::{DesktopConfig, load_desktop_config, release_date};
use crate::desktop::icons::copy_desktop_icons;
use crate::web::config::{WebConfig, load_config};
use askama::Template;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub enum Error {
    Template {
        context: &'static str,
        source: askama::Error,
    },
    Io {
        context: &'static str,
        source: std::io::Error,
    },
    AndroidIcons(crate::android::icons::IconsError),
    DesktopIcons(crate::desktop::icons::IconsError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Template { context, .. } => write!(f, "cannot render {context} template"),
            Self::Io { context, .. } => write!(f, "cannot {context}"),
            Self::AndroidIcons(e) => write!(f, "{e}"),
            Self::DesktopIcons(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Template { source, .. } => Some(source),
            Self::Io { source, .. } => Some(source),
            Self::AndroidIcons(e) => Some(e),
            Self::DesktopIcons(e) => Some(e),
        }
    }
}

impl From<crate::android::icons::IconsError> for Error {
    fn from(e: crate::android::icons::IconsError) -> Self {
        Self::AndroidIcons(e)
    }
}

impl From<crate::desktop::icons::IconsError> for Error {
    fn from(e: crate::desktop::icons::IconsError) -> Self {
        Self::DesktopIcons(e)
    }
}

#[derive(Debug, Clone)]
pub struct BuildConfigs {
    pub android: AndroidConfig,
    pub web: WebConfig,
    pub desktop: DesktopConfig,
}

fn render<T: Template>(template: &T, context: &'static str) -> Result<String, Error> {
    template
        .render()
        .map_err(|source| Error::Template { context, source })
}

fn write_file(file: impl AsRef<Path>, contents: &str) -> Result<(), Error> {
    let path = file.as_ref();
    if let Some(dir) = path.parent() {
        create_dir(dir)?;
    }
    std::fs::write(path, contents).map_err(|source| Error::Io {
        context: "write generated file",
        source,
    })
}

fn create_dir(dir: &Path) -> Result<(), Error> {
    std::fs::create_dir_all(dir).map_err(|source| Error::Io {
        context: "create directory",
        source,
    })
}

fn remove_dir_all_if_exists(dir: &Path) -> Result<(), Error> {
    match std::fs::remove_dir_all(dir) {
        Ok(()) => Ok(()),
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(source) => Err(Error::Io {
            context: "remove stale directory",
            source,
        }),
    }
}

fn rerun_if_changed(path: impl AsRef<Path>) {
    println!("cargo:rerun-if-changed={}", path.as_ref().display());
}

fn emit_rerun_if_changed(egui_root: &Path) {
    rerun_if_changed("Cargo.toml");
    [
        "assets/favicon/mipmap-mdpi.png",
        "assets/favicon/mipmap-hdpi.png",
        "assets/favicon/mipmap-xhdpi.png",
        "assets/favicon/mipmap-xxhdpi.png",
        "assets/favicon/mipmap-xxxhdpi.png",
        "assets/icon.png",
    ]
    .into_iter()
    .for_each(rerun_if_changed);
    [
        "assets/web/egui.js",
        "templates/web/index.html",
        "templates/web/manifest.json",
        "templates/android/build.gradle",
        "templates/android/settings.gradle",
        "templates/android/gradle.properties",
        "templates/android/app/build.gradle",
        "templates/android/app/src/main/AndroidManifest.xml",
        "templates/android/app/src/main/java/MainActivity.java",
        "templates/android/app/src/main/res/values/styles.xml",
        "templates/android/app/src/main/java/Waker.java",
        "templates/desktop/app.desktop",
        "templates/desktop/metainfo.xml",
    ]
    .into_iter()
    .map(|rel| egui_root.join(rel))
    .for_each(rerun_if_changed);
}

fn write_android(android_cfg: &AndroidConfig) -> Result<(), Error> {
    let settings = render(
        &crate::android::templates::SettingsGradle {
            app_name: &android_cfg.app_name,
        },
        "android settings.gradle",
    )?;
    let app_build = render(
        &crate::android::templates::AppBuildGradle {
            namespace: &android_cfg.namespace,
            application_id: &android_cfg.application_id,
            version_code: android_cfg.version_code,
            version_name: &android_cfg.version_name,
        },
        "android app build.gradle",
    )?;
    let manifest = render(
        &crate::android::templates::ManifestXml {
            label: &android_cfg.label,
            activity_fqn: &android_cfg.activity_fqn,
            lib_name: &android_cfg.lib_name,
            host: &android_cfg.host,
            path_prefix: &android_cfg.path_prefix,
            extra_intent_filters: &android_cfg.extra_intent_filters,
            camera: android_cfg.camera,
            internet: android_cfg.internet,
        },
        "android manifest",
    )?;
    let java = render(
        &crate::android::templates::MainActivity {
            package: &android_cfg.namespace,
            lib_name: &android_cfg.lib_name,
        },
        "android MainActivity",
    )?;
    let root_build = render(
        &crate::android::templates::RootBuildGradle,
        "android root build.gradle",
    )?;
    let gradle_props = render(
        &crate::android::templates::GradleProperties,
        "android gradle.properties",
    )?;
    let styles = render(&crate::android::templates::Styles, "android styles.xml")?;
    let waker = render(&crate::android::templates::Waker, "android Waker")?;

    write_file("android/build.gradle", &root_build)?;
    write_file("android/settings.gradle", &settings)?;
    write_file("android/gradle.properties", &gradle_props)?;
    write_file("android/app/src/main/res/values/styles.xml", &styles)?;
    write_file("android/app/build.gradle", &app_build)?;
    write_file("android/app/src/main/AndroidManifest.xml", &manifest)?;
    remove_dir_all_if_exists(Path::new("android/app/src/main/java"))?;
    let java_dir = PathBuf::from(format!(
        "android/app/src/main/java/{}",
        android_cfg.namespace.replace('.', "/")
    ));
    write_file(java_dir.join("MainActivity.java"), &java)?;
    write_file("android/app/src/main/java/com/functora/Waker.java", &waker)?;
    if android_cfg.camera {
        remove_dir_all_if_exists(Path::new("android/app/src/main/java/functora"))?;
    }
    let _icons = copy_launcher_icons(Path::new("android"), Path::new("assets/favicon"))?;
    Ok(())
}

fn write_desktop(desktop_cfg: &DesktopConfig) -> Result<(), Error> {
    let desktop_entry = render(
        &crate::desktop::templates::LinuxDesktop {
            title: &desktop_cfg.title,
            comment: &desktop_cfg.comment,
            exec_name: &desktop_cfg.exec_name,
            icon_name: &desktop_cfg.icon_name,
            categories: &desktop_cfg.categories,
            mime_types: &desktop_cfg.mime_types,
            schemes: &desktop_cfg.schemes,
        },
        "desktop entry",
    )?;
    let release_date = release_date();
    let metainfo = render(
        &crate::desktop::templates::Metainfo {
            app_id: &desktop_cfg.app_id,
            title: &desktop_cfg.title,
            comment: &desktop_cfg.comment,
            description: &desktop_cfg.description,
            version: &desktop_cfg.version,
            date: &release_date,
            homepage: &desktop_cfg.homepage,
            developer: &desktop_cfg.developer,
            developer_id: &desktop_cfg.developer_id,
        },
        "desktop metainfo",
    )?;
    write_file(
        format!("desktop/linux/{}.desktop", desktop_cfg.app_id),
        &desktop_entry,
    )?;
    write_file(
        format!("desktop/metainfo/{}.metainfo.xml", desktop_cfg.app_id),
        &metainfo,
    )?;
    let _icons = copy_desktop_icons(
        Path::new("desktop"),
        Path::new("assets"),
        &desktop_cfg.app_id,
    )?;
    Ok(())
}

fn write_web(web_cfg: &WebConfig, egui_root: &Path) -> Result<(), Error> {
    let index = render(
        &crate::web::templates::IndexHtml {
            title: &web_cfg.title,
            theme_color: &web_cfg.theme_color,
            pkg_js: &web_cfg.pkg_js,
            vsn: &web_cfg.vsn,
        },
        "web index.html",
    )?;
    let manifest_web = render(
        &crate::web::templates::ManifestJson {
            title: &web_cfg.title,
            short_name: &web_cfg.short_name,
            theme_color: &web_cfg.theme_color,
        },
        "web manifest",
    )?;
    write_file("assets/index.html", &index)?;
    write_file("assets/manifest.webmanifest", &manifest_web)?;
    let _bytes_copied = std::fs::copy(egui_root.join("assets/web/egui.js"), "assets/egui.js")
        .map_err(|source| Error::Io {
            context: "copy egui.js web asset",
            source,
        })?;
    Ok(())
}

pub fn run(manifest: &str, egui_root: &Path) -> Result<BuildConfigs, Error> {
    emit_rerun_if_changed(egui_root);
    let android_cfg = load_android_config(manifest);
    let web_cfg = load_config(manifest);
    let desktop_cfg = load_desktop_config(manifest);
    write_android(&android_cfg)?;
    write_desktop(&desktop_cfg)?;
    if std::env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default() == "wasm32" {
        write_web(&web_cfg, egui_root)?;
    }
    Ok(BuildConfigs {
        android: android_cfg,
        web: web_cfg,
        desktop: desktop_cfg,
    })
}
