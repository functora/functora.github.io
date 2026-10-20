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
    package_str(value, "name").map(ToOwned::to_owned)
}

#[must_use]
pub fn lib_name_from(value: &toml::Value) -> Option<String> {
    value
        .get("lib")
        .and_then(|lib| lib.get("name"))
        .and_then(|n| n.as_str())
        .map(ToOwned::to_owned)
}

pub(crate) fn package_str<'a>(value: &'a toml::Value, key: &str) -> Option<&'a str> {
    value
        .get("package")
        .and_then(|pkg| pkg.get(key))
        .and_then(|v| v.as_str())
}

pub(crate) fn metadata_str_in(value: &toml::Value, table: &str, key: &str) -> Option<String> {
    value
        .get("package")
        .and_then(|pkg| pkg.get("metadata"))
        .and_then(|meta| meta.get(table))
        .and_then(|t| t.get(key))
        .and_then(|v| v.as_str())
        .map(ToOwned::to_owned)
}

pub(crate) fn metadata_bool_in(value: &toml::Value, table: &str, key: &str) -> bool {
    value
        .get("package")
        .and_then(|pkg| pkg.get("metadata"))
        .and_then(|meta| meta.get(table))
        .and_then(|t| t.get(key))
        .and_then(toml::Value::as_bool)
        .unwrap_or(false)
}

pub(crate) fn derive_pkg_js_in(value: &toml::Value) -> String {
    lib_name_from(value).map_or_else(
        || {
            pkg_name(value).map_or_else(
                || "app.js".to_owned(),
                |name| format!("{}.js", name.replace('-', "_")),
            )
        },
        |name| format!("{name}.js"),
    )
}

pub(crate) fn derive_lib_name_in(value: &toml::Value) -> String {
    lib_name_from(value)
        .or_else(|| pkg_name(value).map(|name| name.replace('-', "_")))
        .unwrap_or_else(|| "app".to_owned())
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
    parse_toml(manifest_path).map_or_else(|| "app.js".to_owned(), |value| derive_pkg_js_in(&value))
}

#[must_use]
pub fn derive_lib_name(manifest_path: &str) -> String {
    parse_toml(manifest_path).map_or_else(|| "app".to_owned(), |value| derive_lib_name_in(&value))
}

#[must_use]
pub fn metadata_str(manifest_path: &str, table: &str, key: &str) -> Option<String> {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| metadata_str_in(value, table, key))
}

#[must_use]
pub fn metadata_bool(manifest_path: &str, table: &str, key: &str) -> bool {
    parse_toml(manifest_path)
        .as_ref()
        .is_some_and(|value| metadata_bool_in(value, table, key))
}
