#![cfg(any(feature = "build", feature = "android"))]
use askama::Template;
use functora_egui::android::templates::ManifestXml;
use functora_egui::android::{AndroidConfig, load_android_config};

const MANIFEST_TEMPLATE: &str =
    include_str!("../templates/android/app/src/main/AndroidManifest.xml");
const TEMPLATES_SRC: &str = include_str!("../src/android/templates.rs");
const RUN_SRC: &str = include_str!("../src/android/run.rs");

const EXTRA_FILTERS: &str = r#"<intent-filter>
    <action android:name="android.intent.action.VIEW" />
    <category android:name="android.intent.category.DEFAULT" />
    <data android:scheme="content" android:mimeType="application/octet-stream" android:pathPattern=".*\.cryptonote" />
    <data android:scheme="file" android:mimeType="*/*" android:pathPattern=".*\.cryptonote" />
</intent-filter>"#;

fn config(extra_filters: Option<&str>) -> AndroidConfig {
    let metadata = extra_filters.map_or_else(String::new, |extra| {
        format!("extra_intent_filters = '''\n{extra}\n'''\n")
    });
    config_metadata(&metadata)
}

fn config_metadata(metadata: &str) -> AndroidConfig {
    let tmp = tempfile::NamedTempFile::new()
        .unwrap_or_else(|err| panic!("temp manifest file must be created: {err}"));
    let path = tmp
        .path()
        .to_str()
        .unwrap_or_else(|| panic!("temp path must be UTF-8"));
    std::fs::write(
        path,
        format!(
            "[package]\nname = \"deep-link-app\"\nversion = \"1.0.0\"\n[package.metadata.functora-egui-android]\n{metadata}"
        ),
    )
    .unwrap_or_else(|err| panic!("temp manifest must be written: {err}"));
    load_android_config(path)
}

fn render(cfg: &AndroidConfig) -> String {
    ManifestXml {
        label: &cfg.label,
        activity_fqn: &cfg.activity_fqn,
        lib_name: &cfg.lib_name,
        host: &cfg.host,
        path_prefix: &cfg.path_prefix,
        extra_intent_filters: &cfg.extra_intent_filters,
        camera: cfg.camera,
        internet: cfg.internet,
    }
    .render()
    .unwrap_or_else(|err| panic!("android manifest must render: {err}"))
}

#[test]
fn extra_intent_filters_are_indented_block() {
    let cfg = config(Some(EXTRA_FILTERS));
    assert!(
        cfg.extra_intent_filters
            .starts_with("            <intent-filter>")
    );
    assert!(cfg.extra_intent_filters.ends_with('\n'));
    assert!(
        cfg.extra_intent_filters
            .lines()
            .all(|line| line.starts_with("            "))
    );
    assert!(
        cfg.extra_intent_filters
            .contains(r#"android:pathPattern=".*\.cryptonote""#)
    );
}

#[test]
fn missing_extra_key_yields_empty_block() {
    let cfg = config(None);
    assert!(cfg.extra_intent_filters.is_empty());
    let manifest = render(&cfg);
    assert!(!manifest.contains("\n\n        </activity>"));
    assert!(manifest.contains("\n        </activity>"));
}

#[test]
fn rendered_manifest_embeds_extra_filters_inside_activity() {
    let cfg = config(Some(EXTRA_FILTERS));
    let manifest = render(&cfg);
    assert!(!manifest.contains("{{"));
    let pattern = manifest
        .find(r#"android:pathPattern=""#)
        .unwrap_or_else(|| panic!("pathPattern filter must render"));
    let activity_close = manifest
        .find("</activity>")
        .unwrap_or_else(|| panic!("activity end tag must render"));
    assert!(pattern < activity_close);
    assert!(manifest.contains(r#"android:autoVerify="true""#));
    assert!(manifest.contains(r#"android:launchMode="singleInstance""#));
    assert_eq!(manifest.matches("BROWSABLE").count(), 1);
    assert_eq!(manifest.matches("category.DEFAULT").count(), 2);
}

#[test]
fn manifest_template_keeps_extra_slot_raw() {
    assert!(MANIFEST_TEMPLATE.contains("{{ extra_intent_filters }}        </activity>"));
    assert!(MANIFEST_TEMPLATE.contains(r#"android:autoVerify="true""#));
    assert!(TEMPLATES_SRC.contains("escape = \"none\""));
    assert!(TEMPLATES_SRC.contains("path = \"android/app/src/main/AndroidManifest.xml\""));
}

#[test]
fn android_run_schedules_repaint_on_deep_link_update() {
    assert!(RUN_SRC.contains("set_schedule_update"));
    assert!(RUN_SRC.contains("wake_via_repaint"));
}

#[test]
fn manifest_template_declares_tools_and_strips_internet() {
    assert!(MANIFEST_TEMPLATE.contains(r#"xmlns:tools="http://schemas.android.com/tools""#));
    assert!(MANIFEST_TEMPLATE.contains(
        r#"<uses-permission android:name="android.permission.INTERNET" tools:node="remove" />"#
    ));
}

#[test]
fn rendered_manifest_strips_internet_permission() {
    for camera in [false, true] {
        let mut cfg = config(None);
        cfg.camera = camera;
        let manifest = render(&cfg);
        let internet: Vec<&str> = manifest
            .lines()
            .filter(|line| line.contains("android.permission.INTERNET"))
            .collect();
        assert_eq!(internet.len(), 1, "camera={camera}: {manifest}");
        assert!(
            internet[0].contains(r#"tools:node="remove""#),
            "camera={camera}: bare INTERNET permission: {}",
            internet[0]
        );
    }
}

#[test]
fn rendered_manifest_permissions_are_exactly_expected() {
    let mut cfg = config(None);
    cfg.camera = true;
    let manifest = render(&cfg);
    let permissions: Vec<&str> = manifest
        .lines()
        .map(str::trim)
        .filter(|line| line.starts_with("<uses-permission"))
        .collect();
    assert_eq!(permissions.len(), 2, "{manifest}");
    assert!(
        permissions
            .iter()
            .any(|perm| perm.contains("android.permission.INTERNET")
                && perm.contains(r#"tools:node="remove""#))
    );
    assert!(
        permissions
            .iter()
            .any(|perm| perm.contains(r#"android.permission.CAMERA""#)
                && !perm.contains("tools:node"))
    );
}

#[test]
fn default_config_is_offline() {
    let cfg = config(None);
    assert!(!cfg.internet);
    let manifest = render(&cfg);
    assert!(manifest.contains(
        r#"<uses-permission android:name="android.permission.INTERNET" tools:node="remove" />"#
    ));
    assert!(!manifest.contains(r#"android.permission.INTERNET" />"#));
}

#[test]
fn internet_metadata_grants_internet() {
    let mut cfg = config_metadata("internet = true\n");
    assert!(cfg.internet);
    cfg.camera = true;
    let manifest = render(&cfg);
    assert!(
        manifest.contains(r#"<uses-permission android:name="android.permission.INTERNET" />"#),
        "{manifest}"
    );
    assert!(!manifest.contains(r#"tools:node=""#), "{manifest}");
    let permissions: Vec<&str> = manifest
        .lines()
        .map(str::trim)
        .filter(|line| line.starts_with("<uses-permission"))
        .collect();
    assert_eq!(permissions.len(), 2, "{manifest}");
    assert!(permissions.iter().any(
        |perm| perm.contains(r#"android.permission.INTERNET""#) && !perm.contains("tools:node")
    ));
    assert!(
        permissions
            .iter()
            .any(|perm| perm.contains(r#"android.permission.CAMERA""#))
    );
}
