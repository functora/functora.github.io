use crate::config::shared::{
    capitalize_words, derive_pkg_js_in, metadata_str_in, package_str, parse_toml, pkg_name,
};
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

fn derive_title_in(value: &toml::Value) -> String {
    metadata_str_in(value, "functora-egui-web", "title").unwrap_or_else(|| {
        pkg_name(value).map_or_else(|| "App".to_owned(), |name| capitalize_words(&name))
    })
}

fn derive_theme_color_in(value: &toml::Value) -> String {
    metadata_str_in(value, "functora-egui-web", "theme_color").unwrap_or_else(default_theme_color)
}

#[must_use]
pub fn derive_title(manifest_path: &str) -> String {
    let value =
        parse_toml(manifest_path).unwrap_or_else(|| toml::Value::Table(toml::map::Map::new()));
    derive_title_in(&value)
}

#[must_use]
pub fn derive_theme_color(manifest_path: &str) -> String {
    let value =
        parse_toml(manifest_path).unwrap_or_else(|| toml::Value::Table(toml::map::Map::new()));
    derive_theme_color_in(&value)
}

#[must_use]
pub fn load_config(manifest_path: &str) -> WebConfig {
    let value =
        parse_toml(manifest_path).unwrap_or_else(|| toml::Value::Table(toml::map::Map::new()));
    let pkg_name = pkg_name(&value).unwrap_or_else(|| "app".to_owned());
    let short_name = metadata_str_in(&value, "functora-egui-web", "short_name")
        .unwrap_or_else(|| pkg_name.clone());
    WebConfig {
        title: derive_title_in(&value),
        short_name,
        theme_color: derive_theme_color_in(&value),
        pkg_js: derive_pkg_js_in(&value),
        vsn: package_str(&value, "version").unwrap_or("0.0.0").to_owned(),
    }
}
