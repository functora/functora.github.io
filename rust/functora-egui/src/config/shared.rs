#[must_use]
pub fn capitalize_words(input: &str) -> String {
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

#[must_use]
pub fn parse_toml(path: &str) -> Option<toml::Value> {
    std::fs::read_to_string(path)
        .ok()
        .and_then(|content| content.parse::<toml::Value>().ok())
}

#[must_use]
pub fn pkg_name(value: &toml::Value) -> Option<String> {
    value
        .get("package")
        .and_then(|pkg| pkg.get("name"))
        .and_then(|n| n.as_str())
        .map(ToOwned::to_owned)
}

#[must_use]
pub fn lib_name_from(value: &toml::Value) -> Option<String> {
    value
        .get("lib")
        .and_then(|lib| lib.get("name"))
        .and_then(|n| n.as_str())
        .map(ToOwned::to_owned)
}

#[must_use]
pub fn app_name_from_manifest(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(pkg_name)
        .unwrap_or_else(|| "app".to_owned())
}

#[must_use]
pub fn derive_pkg_js(manifest_path: &str) -> String {
    parse_toml(manifest_path).map_or_else(
        || "app.js".to_owned(),
        |value| {
            value
                .get("lib")
                .and_then(|lib| lib.get("name"))
                .and_then(|n| n.as_str())
                .map_or_else(
                    || {
                        value
                            .get("package")
                            .and_then(|pkg| pkg.get("name"))
                            .and_then(|n| n.as_str())
                            .map_or_else(
                                || "app.js".to_owned(),
                                |name| format!("{}.js", name.replace('-', "_")),
                            )
                    },
                    |name| format!("{name}.js"),
                )
        },
    )
}

#[must_use]
pub fn derive_lib_name(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(lib_name_from)
        .unwrap_or_else(|| {
            parse_toml(manifest_path)
                .as_ref()
                .and_then(pkg_name)
                .map_or_else(|| "app".to_owned(), |name| name.replace('-', "_"))
        })
}

pub fn metadata_str(manifest_path: &str, table: &str, key: &str) -> Option<String> {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get(table))
                .and_then(|t| t.get(key))
                .and_then(|v| v.as_str())
        })
        .map(ToOwned::to_owned)
}
