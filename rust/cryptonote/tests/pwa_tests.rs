use serde_json::Value;

fn parse_manifest(json: &str) -> Value {
    serde_json::from_str(json).unwrap_or_else(|e| panic!("manifest must be valid JSON: {e}"))
}

#[test]
fn manifest_derives_name_and_cache_name() {
    let attrs = cryptonote::APP_ATTRS;
    let parsed = parse_manifest(&functora_dioxus::manifest_json(
        attrs.app,
        attrs.vsn,
        attrs.description,
        "https://host/apps/cryptonote/0.1.10/",
        "https://host/apps/cryptonote/0.1.10/",
        &[],
    ));
    let obj = parsed
        .as_object()
        .unwrap_or_else(|| panic!("manifest must be an object"));
    assert_eq!(obj.get("name").and_then(|v| v.as_str()), Some("Cryptonote"));
    assert_eq!(obj.get("short_name").and_then(|v| v.as_str()), Some("Cryptonote"));
    assert_eq!(
        obj.get("cache_name").and_then(|v| v.as_str()),
        Some(attrs.cache_name().as_str())
    );
    assert_eq!(obj.get("description").and_then(|v| v.as_str()), Some(attrs.description));
}

#[test]
fn cache_name_is_derived_from_app_and_version() {
    let attrs = cryptonote::APP_ATTRS;
    assert_eq!(attrs.cache_name(), format!("{}-v{}", attrs.app, attrs.vsn));
    assert_eq!(attrs.cache_name(), "cryptonote-v0.1.10");
}

#[test]
fn service_worker_registration_uses_derived_cache() {
    let js = functora_dioxus::pwa_init_js("/sw.js", &cryptonote::APP_ATTRS.cache_name());
    assert!(js.contains("serviceWorker"));
    assert!(js.contains(&format!("/sw.js?cache={}", cryptonote::APP_ATTRS.cache_name())));
}

#[test]
fn app_attrs_are_consistent() {
    let attrs = cryptonote::APP_ATTRS;
    assert_eq!(attrs.app, "cryptonote");
    assert_eq!(attrs.vsn, env!("CARGO_PKG_VERSION"));
    assert_eq!(attrs.org, "functora");
    assert_eq!(attrs.app_name(), "Cryptonote");
}
