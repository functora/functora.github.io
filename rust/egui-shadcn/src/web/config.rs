use crate::theme::shadcn_theme_dark::dark;

#[derive(Debug, Clone)]
pub struct WebConfig {
    pub title: String,
    pub short_name: String,
    pub theme_color: String,
    pub pkg_js: String,
    pub vsn: String,
}

fn capitalize_words(input: &str) -> String {
    input
        .split('-')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn default_theme_color() -> String {
    let bg = dark().background;
    format!("#{:02x}{:02x}{:02x}", bg.r(), bg.g(), bg.b())
}

#[must_use]
pub fn derive_pkg_js(manifest_path: &str) -> String {
    std::fs::read_to_string(manifest_path)
        .ok()
        .and_then(|content| content.parse::<toml::Value>().ok())
        .map_or_else(
            || "app.js".to_owned(),
            |value| {
                value
                    .get("lib")
                    .and_then(|lib| lib.get("name"))
                    .and_then(|name| name.as_str())
                    .map(|name| format!("{name}.js"))
                    .unwrap_or_else(|| {
                        value
                            .get("package")
                            .and_then(|pkg| pkg.get("name"))
                            .and_then(|name| name.as_str())
                            .map_or_else(
                                || "app.js".to_owned(),
                                |name| format!("{}.js", name.replace('-', "_")),
                            )
                    })
            },
        )
}

#[must_use]
pub fn derive_title(manifest_path: &str) -> String {
    std::fs::read_to_string(manifest_path)
        .ok()
        .and_then(|content| content.parse::<toml::Value>().ok())
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get("egui-web"))
                .and_then(|web| web.get("title"))
                .and_then(|t| t.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(|| {
            std::fs::read_to_string(manifest_path)
                .ok()
                .and_then(|content| content.parse::<toml::Value>().ok())
                .and_then(|value| {
                    value
                        .get("package")
                        .and_then(|pkg| pkg.get("name"))
                        .and_then(|name| name.as_str())
                        .map(|name| capitalize_words(name))
                })
                .unwrap_or_else(|| "App".to_owned())
        })
}

#[must_use]
pub fn derive_theme_color(manifest_path: &str) -> String {
    std::fs::read_to_string(manifest_path)
        .ok()
        .and_then(|content| content.parse::<toml::Value>().ok())
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get("egui-web"))
                .and_then(|web| web.get("theme_color"))
                .and_then(|c| c.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(default_theme_color)
}

#[must_use]
pub fn load_config(manifest_path: &str) -> WebConfig {
    let content = std::fs::read_to_string(manifest_path).unwrap_or_default();
    let value: toml::Value = content.parse().unwrap_or(toml::Value::Table(toml::map::Map::new()));
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
        .and_then(|meta| meta.get("egui-web"))
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
