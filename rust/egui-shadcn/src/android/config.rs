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

fn parse_toml(path: &str) -> Option<toml::Value> {
    std::fs::read_to_string(path)
        .ok()
        .and_then(|content| content.parse::<toml::Value>().ok())
}

fn pkg_name(value: &toml::Value) -> Option<String> {
    value
        .get("package")
        .and_then(|pkg| pkg.get("name"))
        .and_then(|n| n.as_str())
        .map(ToOwned::to_owned)
}

fn lib_name_from(value: &toml::Value) -> Option<String> {
    value
        .get("lib")
        .and_then(|lib| lib.get("name"))
        .and_then(|n| n.as_str())
        .map(ToOwned::to_owned)
}

fn derive_namespace(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get("egui-android"))
                .and_then(|a| a.get("namespace"))
                .and_then(|n| n.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(|| {
            parse_toml(manifest_path)
                .as_ref()
                .and_then(pkg_name)
                .map_or_else(
                    || "com.functora.app".to_owned(),
                    |name| format!("com.functora.{}", name.replace('-', "_")),
                )
        })
}

fn derive_label(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get("egui-android"))
                .and_then(|a| a.get("label"))
                .and_then(|t| t.as_str())
                .map(ToOwned::to_owned)
                .or_else(|| {
                    value
                        .get("package")
                        .and_then(|pkg| pkg.get("metadata"))
                        .and_then(|meta| meta.get("egui-web"))
                        .and_then(|w| w.get("title"))
                        .and_then(|t| t.as_str())
                        .map(ToOwned::to_owned)
                })
        })
        .unwrap_or_else(|| {
            parse_toml(manifest_path)
                .as_ref()
                .and_then(pkg_name)
                .map_or_else(|| "App".to_owned(), |name| capitalize_words(&name))
        })
}

fn derive_lib_name(manifest_path: &str) -> String {
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

fn derive_host(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get("egui-android"))
                .and_then(|a| a.get("host"))
                .and_then(|h| h.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(|| "functora.github.io".to_owned())
}

fn derive_path_prefix(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get("egui-android"))
                .and_then(|a| a.get("path_prefix"))
                .and_then(|p| p.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(|| {
            parse_toml(manifest_path)
                .as_ref()
                .and_then(pkg_name)
                .map_or_else(|| "/apps/app/".to_owned(), |name| format!("/apps/{name}/"))
        })
}

fn derive_extra_intent_filters(manifest_path: &str) -> String {
    parse_toml(manifest_path)
        .as_ref()
        .and_then(|value| {
            value
                .get("package")
                .and_then(|pkg| pkg.get("metadata"))
                .and_then(|meta| meta.get("egui-android"))
                .and_then(|a| a.get("extra_intent_filters"))
                .and_then(|f| f.as_str())
                .map(ToOwned::to_owned)
        })
        .unwrap_or_default()
        .trim()
        .to_owned()
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
    let namespace = derive_namespace(manifest_path);
    let label = derive_label(manifest_path);
    let lib_name = derive_lib_name(manifest_path);
    let host = derive_host(manifest_path);
    let path_prefix = derive_path_prefix(manifest_path);
    let extra_raw = derive_extra_intent_filters(manifest_path);
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
    let version_name = derive_version(manifest_path);
    let version_code = version_code_from(&version_name);
    let activity_fqn = format!("{namespace}.MainActivity");
    let app_name = parse_toml(manifest_path)
        .as_ref()
        .and_then(pkg_name)
        .unwrap_or_else(|| "app".to_owned());
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
    }
}
