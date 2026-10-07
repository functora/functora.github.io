use crate::config::shared::{capitalize_words, metadata_str, parse_toml, pkg_name};

#[derive(Debug, Clone)]
pub struct DesktopConfig {
    pub app_id: String,
    pub title: String,
    pub comment: String,
    pub categories: String,
    pub mime_types: String,
    pub schemes: String,
    pub version: String,
    pub exec_name: String,
    pub icon_name: String,
}

fn derive_app_id(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "app_id").unwrap_or_else(|| {
        parse_toml(manifest_path)
            .as_ref()
            .and_then(pkg_name)
            .map_or_else(
                || "io.functora.app".to_owned(),
                |name| format!("io.functora.{}", name.replace('-', "_")),
            )
    })
}

fn derive_title(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "title")
        .or_else(|| metadata_str(manifest_path, "functora-egui-web", "title"))
        .unwrap_or_else(|| {
            parse_toml(manifest_path)
                .as_ref()
                .and_then(pkg_name)
                .map_or_else(|| "App".to_owned(), |name| capitalize_words(&name))
        })
}

fn derive_comment(manifest_path: &str, title: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "comment").unwrap_or_else(|| {
        parse_toml(manifest_path)
            .as_ref()
            .and_then(|value| {
                value
                    .get("package")
                    .and_then(|pkg| pkg.get("description"))
                    .and_then(|d| d.as_str())
            })
            .map_or_else(|| title.to_owned(), ToOwned::to_owned)
    })
}

fn derive_categories(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "categories")
        .unwrap_or_else(|| "Utility;".to_owned())
}

fn derive_mime_types(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "mime_types").unwrap_or_default()
}

fn derive_schemes(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-desktop", "schemes").unwrap_or_default()
}

fn derive_version(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("version"))
                .and_then(|v| v.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(|| "0.0.0".to_owned())
}

fn derive_exec_name(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(pkg_name)
        .unwrap_or_else(|| "app".to_owned())
}

#[must_use]
pub fn load_desktop_config(manifest_path: &str) -> DesktopConfig {
    let app_id = derive_app_id(manifest_path);
    let title = derive_title(manifest_path);
    let comment = derive_comment(manifest_path, &title);
    DesktopConfig {
        icon_name: app_id.clone(),
        app_id,
        exec_name: derive_exec_name(manifest_path),
        version: derive_version(manifest_path),
        categories: derive_categories(manifest_path),
        mime_types: derive_mime_types(manifest_path),
        schemes: derive_schemes(manifest_path),
        comment,
        title,
    }
}
