use functora_dioxus::{ManifestIcon, manifest_json, pwa_init_js};
use serde_json::Value;

fn parse_manifest(json: &str) -> Value {
    serde_json::from_str(json).unwrap_or_else(|e| panic!("manifest must be valid JSON: {e}"))
}

fn must_object<'a>(val: &'a Value, label: &str) -> &'a serde_json::Map<String, Value> {
    match val.as_object() {
        Some(obj) => obj,
        None => panic!("{label} must be an object"),
    }
}

fn must_array<'a>(val: &'a Value, label: &str) -> &'a Vec<Value> {
    match val.as_array() {
        Some(arr) => arr,
        None => panic!("{label} must be an array"),
    }
}

fn must_str<'a>(val: &'a Value, label: &str) -> &'a str {
    match val.as_str() {
        Some(s) => s,
        None => panic!("{label} must be a string"),
    }
}

fn sample_manifest() -> String {
    manifest_json(
        "cryptonote",
        "0.1.10",
        "Encrypted offline notes with file attachments.",
        "https://host/apps/cryptonote/0.1.10/",
        "https://host/apps/cryptonote/0.1.10/",
        &[
            ManifestIcon {
                src: "https://host/apps/cryptonote/0.1.10/assets/a192.png".to_string(),
                sizes: "192x192",
                r#type: "image/png",
                purpose: "any",
            },
            ManifestIcon {
                src: "https://host/apps/cryptonote/0.1.10/assets/a512.png".to_string(),
                sizes: "512x512",
                r#type: "image/png",
                purpose: "any",
            },
        ],
    )
}

#[test]
fn manifest_json_is_valid_json() {
    let parsed = parse_manifest(&sample_manifest());
    assert!(parsed.is_object());
}

#[test]
fn manifest_json_derives_name_and_cache_name() {
    let binding = parse_manifest(&sample_manifest());
    let obj = must_object(&binding, "manifest");
    assert_eq!(must_str(obj.get("name").unwrap_or(&Value::Null), "name"), "Cryptonote");
    assert_eq!(
        must_str(obj.get("short_name").unwrap_or(&Value::Null), "short_name"),
        "Cryptonote"
    );
    assert_eq!(
        must_str(obj.get("cache_name").unwrap_or(&Value::Null), "cache_name"),
        "cryptonote-v0.1.10"
    );
}

#[test]
fn manifest_json_contains_required_fields() {
    let binding = parse_manifest(&sample_manifest());
    let obj = must_object(&binding, "manifest");
    assert_eq!(
        must_str(obj.get("description").unwrap_or(&Value::Null), "description"),
        "Encrypted offline notes with file attachments."
    );
    assert_eq!(
        must_str(obj.get("display").unwrap_or(&Value::Null), "display"),
        "standalone"
    );
    assert_eq!(
        must_str(obj.get("theme_color").unwrap_or(&Value::Null), "theme_color"),
        "#679"
    );
    assert_eq!(
        must_str(obj.get("background_color").unwrap_or(&Value::Null), "background_color"),
        "#ffffff"
    );
    assert_eq!(
        must_str(obj.get("start_url").unwrap_or(&Value::Null), "start_url"),
        "https://host/apps/cryptonote/0.1.10/"
    );
    assert_eq!(
        must_str(obj.get("scope").unwrap_or(&Value::Null), "scope"),
        "https://host/apps/cryptonote/0.1.10/"
    );
}

#[test]
fn manifest_json_has_icons_with_correct_sizes() {
    let binding = parse_manifest(&sample_manifest());
    let icons = must_array(
        must_object(&binding, "manifest").get("icons").unwrap_or(&Value::Null),
        "icons",
    );
    assert_eq!(icons.len(), 2);
    let sizes: Vec<String> = icons
        .iter()
        .map(|i| i.get("sizes").and_then(|v| v.as_str()).unwrap_or("").to_string())
        .collect();
    assert!(sizes.contains(&"192x192".to_string()));
    assert!(sizes.contains(&"512x512".to_string()));
}

#[test]
fn manifest_json_icon_srcs_are_absolute() {
    let binding = parse_manifest(&sample_manifest());
    let obj = must_object(&binding, "manifest");
    let icons = must_array(obj.get("icons").unwrap_or(&Value::Null), "icons");
    let srcs: Vec<String> = icons
        .iter()
        .map(|i| i.get("src").and_then(|v| v.as_str()).unwrap_or("").to_string())
        .collect();
    assert!(srcs.iter().all(|s| s.starts_with("https://host/")));
}

#[test]
fn manifest_json_icon_types_are_png() {
    let binding = parse_manifest(&sample_manifest());
    let obj = must_object(&binding, "manifest");
    let icons = must_array(obj.get("icons").unwrap_or(&Value::Null), "icons");
    for icon in icons {
        assert_eq!(
            must_str(icon.get("type").unwrap_or(&Value::Null), "icon type"),
            "image/png"
        );
    }
}

#[test]
fn pwa_init_js_contains_service_worker_registration() {
    let code = pwa_init_js("sw.js", "cryptonote-v0.1.10");
    assert!(code.contains("serviceWorker"));
    assert!(code.contains("navigator.serviceWorker.register"));
}

#[test]
fn pwa_init_js_is_guarded_by_feature_detection() {
    let code = pwa_init_js("sw.js", "cryptonote-v0.1.10");
    assert!(code.contains("'serviceWorker' in navigator"));
}

#[test]
fn pwa_init_js_registers_sw_with_derived_cache() {
    let code = pwa_init_js("sw.js", "cryptonote-v0.1.10");
    assert!(code.contains("sw.js?cache=cryptonote-v0.1.10"));
}

#[test]
fn pwa_init_js_contains_install_prompt_setup() {
    let code = pwa_init_js("sw.js", "cryptonote-v0.1.10");
    assert!(code.contains("beforeinstallprompt"));
    assert!(code.contains("__functoraPwaDeferred"));
    assert!(code.contains("appinstalled"));
}

const SW_JS: &str = include_str!("../assets/sw.js");

#[test]
fn service_worker_precaches_app_shell_at_install() {
    assert!(SW_JS.contains("self.registration.scope"));
    assert!(SW_JS.contains("cache.add(self.registration.scope)"));
}

#[test]
fn service_worker_falls_back_to_app_shell_for_navigation() {
    assert!(SW_JS.contains("cache.match(self.registration.scope)"));
}

#[test]
fn service_worker_derives_cache_name_from_its_url() {
    assert!(SW_JS.contains("searchParams.get('cache')"));
}
