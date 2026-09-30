#![cfg(any(feature = "build", feature = "android"))]
#![allow(clippy::unwrap_used, clippy::expect_used)]
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
    let tmp = tempfile::NamedTempFile::new().unwrap();
    let path = tmp.path().to_str().unwrap();
    let metadata = extra_filters.map_or_else(String::new, |extra| {
        format!(
            "\n[package.metadata.functora-egui-android]\nextra_intent_filters = '''\n{extra}\n'''\n"
        )
    });
    std::fs::write(
        path,
        format!("[package]\nname = \"deep-link-app\"\nversion = \"1.0.0\"\n{metadata}"),
    )
    .unwrap();
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
    }
    .render()
    .unwrap()
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
    let pattern = manifest.find(r#"android:pathPattern=""#).unwrap();
    let activity_close = manifest.find("</activity>").unwrap();
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
