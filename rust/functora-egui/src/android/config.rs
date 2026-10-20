use crate::config::shared::{
    capitalize_words, derive_lib_name_in, metadata_bool_in, metadata_str_in, package_str,
    parse_toml, pkg_name,
};

#[derive(Debug, Clone)]
pub struct AndroidConfig {
    pub namespace: String,
    pub application_id: String,
    pub activity_fqn: String,
    pub label: String,
    pub lib_name: String,
    pub version_name: String,
    pub version_code: u32,
    pub host: String,
    pub path_prefix: String,
    pub extra_intent_filters: String,
    pub app_name: String,
    pub camera: bool,
    pub internet: bool,
}

fn derive_namespace(value: &toml::Value) -> String {
    metadata_str_in(value, "functora-egui-android", "namespace").unwrap_or_else(|| {
        pkg_name(value).map_or_else(
            || "com.functora.app".to_owned(),
            |name| format!("com.functora.{}", name.replace('-', "_")),
        )
    })
}

fn derive_label(value: &toml::Value) -> String {
    metadata_str_in(value, "functora-egui-android", "label")
        .or_else(|| metadata_str_in(value, "functora-egui-web", "title"))
        .unwrap_or_else(|| {
            pkg_name(value).map_or_else(|| "App".to_owned(), |name| capitalize_words(&name))
        })
}

fn derive_host(value: &toml::Value) -> String {
    metadata_str_in(value, "functora-egui-android", "host")
        .unwrap_or_else(|| "functora.github.io".to_owned())
}

fn derive_path_prefix(value: &toml::Value) -> String {
    metadata_str_in(value, "functora-egui-android", "path_prefix").unwrap_or_else(|| {
        pkg_name(value).map_or_else(|| "/apps/app/".to_owned(), |name| format!("/apps/{name}/"))
    })
}

fn derive_extra_intent_filters(value: &toml::Value) -> String {
    metadata_str_in(value, "functora-egui-android", "extra_intent_filters")
        .unwrap_or_default()
        .trim()
        .to_owned()
}

fn derive_version(value: &toml::Value) -> String {
    package_str(value, "version").unwrap_or("0.0.0").to_owned()
}

fn version_code_from(version: &str) -> u32 {
    let parts: Vec<u32> = version
        .split('.')
        .map(|p| p.parse::<u32>().unwrap_or(0))
        .collect();
    let major = parts.first().copied().unwrap_or(0);
    let minor = parts.get(1).copied().unwrap_or(0);
    let patch = parts.get(2).copied().unwrap_or(0);
    major
        .saturating_mul(10_000)
        .saturating_add(minor.saturating_mul(100))
        .saturating_add(patch)
}

#[must_use]
pub fn load_android_config(manifest_path: &str) -> AndroidConfig {
    let value =
        parse_toml(manifest_path).unwrap_or_else(|| toml::Value::Table(toml::map::Map::new()));
    let namespace = derive_namespace(&value);
    let label = derive_label(&value);
    let lib_name = derive_lib_name_in(&value);
    let host = derive_host(&value);
    let path_prefix = derive_path_prefix(&value);
    let extra_raw = derive_extra_intent_filters(&value);
    let extra_intent_filters = if extra_raw.is_empty() {
        String::new()
    } else {
        extra_raw.lines().fold(String::new(), |mut acc, line| {
            acc.push_str("            ");
            acc.push_str(line);
            acc.push('\n');
            acc
        })
    };
    let version_name = derive_version(&value);
    let version_code = version_code_from(&version_name);
    let activity_fqn = format!("{namespace}.MainActivity");
    let app_name = pkg_name(&value).unwrap_or_else(|| "app".to_owned());
    let camera = metadata_bool_in(&value, "functora-egui-android", "camera");
    let internet = metadata_bool_in(&value, "functora-egui-android", "internet");
    AndroidConfig {
        namespace: namespace.clone(),
        application_id: namespace,
        activity_fqn,
        label,
        lib_name,
        version_name,
        version_code,
        host,
        path_prefix,
        extra_intent_filters,
        app_name,
        camera,
        internet,
    }
}
