#![cfg(any(feature = "build", feature = "desktop"))]
#![allow(clippy::unwrap_used, clippy::expect_used)]
use functora_egui::desktop::load_desktop_config;

fn write_manifest(body: &str) -> tempfile::NamedTempFile {
    let file = tempfile::NamedTempFile::new().expect("temp manifest");
    std::fs::write(file.path(), body).expect("seed manifest");
    file
}

#[test]
fn defaults_derive_from_package() {
    let file = write_manifest("[package]\nname = \"my-app\"\nversion = \"1.2.3\"\n");
    let path = file.path().to_str().expect("manifest path");
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.app_id, "io.functora.my_app");
    assert_eq!(cfg.title, "My App");
    assert_eq!(cfg.comment, "My App");
    assert_eq!(cfg.categories, "Utility;");
    assert!(cfg.mime_types.is_empty());
    assert!(cfg.schemes.is_empty());
    assert_eq!(cfg.version, "1.2.3");
    assert_eq!(cfg.exec_name, "my-app");
    assert_eq!(cfg.icon_name, cfg.app_id);
}

#[test]
fn web_title_is_fallback_before_pkg_name() {
    let file = write_manifest(
        "[package]\nname = \"my-app\"\nversion = \"0.1.0\"\n[package.metadata.functora-egui-web]\ntitle = \"Web Title\"\n",
    );
    let path = file.path().to_str().expect("manifest path");
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.title, "Web Title");
}

#[test]
fn explicit_metadata_overrides_everything() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"2.0.0\"\ndescription = \"pkg desc\"\n[package.metadata.functora-egui-desktop]\napp_id = \"io.example.Notes\"\ntitle = \"Notes\"\ncomment = \"Take notes\"\ncategories = \"Office;\"\nmime_types = \"application/x-notes;\"\nschemes = \"notes;\"\n",
    );
    let path = file.path().to_str().expect("manifest path");
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.app_id, "io.example.Notes");
    assert_eq!(cfg.title, "Notes");
    assert_eq!(cfg.comment, "Take notes");
    assert_eq!(cfg.categories, "Office;");
    assert_eq!(cfg.mime_types, "application/x-notes;");
    assert_eq!(cfg.schemes, "notes;");
    assert_eq!(cfg.version, "2.0.0");
    assert_eq!(cfg.exec_name, "pkg");
    assert_eq!(cfg.icon_name, "io.example.Notes");
}

#[test]
fn comment_falls_back_to_description() {
    let file = write_manifest(
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\ndescription = \"Pkg description\"\n",
    );
    let path = file.path().to_str().expect("manifest path");
    let cfg = load_desktop_config(path);
    assert_eq!(cfg.comment, "Pkg description");
}
