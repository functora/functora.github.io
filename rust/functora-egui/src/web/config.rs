use crate::config::shared::{capitalize_words, derive_pkg_js, metadata_str, parse_toml};
use crate::theme::shadcn_theme_dark::dark;

#[derive(Debug, Clone)]
pub struct WebConfig {
    pub title: String,
    pub short_name: String,
    pub theme_color: String,
    pub pkg_js: String,
    pub vsn: String,
}

fn default_theme_color() -> String {
    let bg = dark().background;
    format!("#{:02x}{:02x}{:02x}", bg.r(), bg.g(), bg.b())
}

#[must_use]
pub fn derive_title(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-web", "title").unwrap_or_else(|| {
        parse_toml(manifest_path)
            .as_ref()
            .and_then(crate::config::shared::pkg_name)
            .map_or_else(|| "App".to_owned(), |name| capitalize_words(&name))
    })
}

#[must_use]
pub fn derive_theme_color(manifest_path: &str) -> String {
    metadata_str(manifest_path, "functora-egui-web", "theme_color")
        .unwrap_or_else(default_theme_color)
}

#[must_use]
pub fn load_config(manifest_path: &str) -> WebConfig {
    let content = std::fs::read_to_string(manifest_path).unwrap_or_default();
    let value: toml::Value = content
        .parse()
        .unwrap_or_else(|_| toml::Value::Table(toml::map::Map::new()));
    let package = value.get("package");
    let vsn = package
        .and_then(|pkg| pkg.get("version"))
        .and_then(|v| v.as_str())
        .unwrap_or("0.0.0")
        .to_owned();
    let pkg_name = package
        .and_then(|pkg| pkg.get("name"))
        .and_then(|n| n.as_str())
        .unwrap_or("app");
    let short_name = package
        .and_then(|pkg| pkg.get("metadata"))
        .and_then(|meta| meta.get("functora-egui-web"))
        .and_then(|web| web.get("short_name"))
        .and_then(|n| n.as_str())
        .map_or_else(|| pkg_name.to_owned(), ToOwned::to_owned);
    WebConfig {
        title: derive_title(manifest_path),
        short_name,
        theme_color: derive_theme_color(manifest_path),
        pkg_js: derive_pkg_js(manifest_path),
        vsn,
    }
}
