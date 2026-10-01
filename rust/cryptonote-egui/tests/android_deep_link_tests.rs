#![allow(clippy::unwrap_used, clippy::expect_used)]
#[cfg(not(target_arch = "wasm32"))]
use cryptonote_egui::crypto::CipherType;
use cryptonote_egui::{ArchiveSource, has_pending_archive, store_archive, take_archive};
#[cfg(not(target_arch = "wasm32"))]
use cryptonote_egui::{External, Screen, create_archive_package_async, load_archive_async, read_archive_metadata};
#[cfg(not(target_arch = "wasm32"))]
use std::future::Future;
use std::sync::{Mutex, MutexGuard};

static STATE_LOCK: Mutex<()> = Mutex::new(());

fn lock_state() -> MutexGuard<'static, ()> {
    STATE_LOCK.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn crate_file(relative: &str) -> String {
    format!("{}/{relative}", env!("CARGO_MANIFEST_DIR"))
}

fn generated(relative: &str) -> String {
    std::fs::read_to_string(crate_file(relative)).expect("generated file")
}

#[cfg(not(target_arch = "wasm32"))]
fn run_async<T, F>(future: F) -> T
where
    T: Send + 'static,
    F: Future<Output = T> + Send + 'static,
{
    functora_egui::spawn_async(future).recv().expect("task completes")
}

fn generated_manifest() -> String {
    generated("android/app/src/main/AndroidManifest.xml")
}

fn activity_fqn(manifest: &str) -> String {
    manifest
        .lines()
        .find(|line| line.contains(".MainActivity\""))
        .and_then(|line| line.split('"').nth(1))
        .expect("activity fqn")
        .to_owned()
}

fn generated_java() -> String {
    let fqn = activity_fqn(&generated_manifest());
    generated(&format!("android/app/src/main/java/{}.java", fqn.replace('.', "/")))
}

#[test]
fn cryptonote_declares_extra_intent_filters_metadata() {
    assert!(include_str!("../Cargo.toml").contains("extra_intent_filters = '''"));
}

#[test]
fn generated_manifest_registers_cryptonote_view_filters() {
    let manifest = generated_manifest();
    assert!(manifest.contains(r#"android:name="com.functora.cryptonote_egui.MainActivity""#));
    assert!(manifest.contains(r#"android:launchMode="singleInstance""#));
    assert!(manifest.contains(r#"android:pathPattern=".*\.cryptonote""#));
    assert!(manifest.contains(r#"android:scheme="content""#));
    assert!(manifest.contains(r#"android:scheme="file""#));
    assert!(manifest.contains(r#"android:mimeType="application/octet-stream""#));
    assert!(manifest.contains(r#"android:mimeType="*/*""#));
    assert_eq!(manifest.matches("BROWSABLE").count(), 1);
    assert_eq!(manifest.matches("android:autoVerify").count(), 1);
    assert_eq!(manifest.matches("android:host=").count(), 1);
    assert_eq!(manifest.matches("category.DEFAULT").count(), 3);
    let pattern_line = manifest
        .lines()
        .find(|line| line.contains("pathPattern"))
        .expect("path pattern line");
    assert!(pattern_line.starts_with("            "));
    let pattern = manifest.find(r#"android:pathPattern=""#).expect("path pattern");
    let activity_close = manifest.find("</activity>").expect("activity close");
    assert!(pattern < activity_close);
}

#[test]
fn generated_java_wires_deep_link_intake() {
    let java = generated_java();
    assert!(java.contains("protected void onNewIntent(Intent intent)"));
    assert!(java.contains("setIntent(intent);"));
    assert!(java.contains("handleDeepLinkIntent(intent);"));
    assert!(java.contains("handleDeepLinkIntent(getIntent());"));
    assert!(java.contains("private static native void handleDeepLink(String url);"));
    assert!(java.contains("private static native void handleDeepLinkFile(String path);"));
    assert!(java.contains("setData(null)"));
    assert!(java.contains("\"functora-deeplink\""));
    assert!(java.contains("copyToCache(uri)"));
}

#[test]
fn jni_symbols_match_generated_activity_class() {
    let fqn = activity_fqn(&generated_manifest());
    let symbol = format!("Java_{}", fqn.replace('.', "_"));
    let source = include_str!("../src/deep_link.rs");
    assert!(source.contains(&format!("{symbol}_handleDeepLink<")));
    assert!(source.contains(&format!("{symbol}_handleDeepLinkFile<")));
}

#[test]
fn app_checks_pending_archive_before_consuming() {
    let source = include_str!("../src/app.rs");
    let pending = source.find("has_pending_archive()").expect("pending check");
    let take = source.find("take_archive()").expect("take call");
    assert!(pending < take);
}

#[test]
fn store_take_archive_roundtrip_tracks_pending_state() {
    let _guard = lock_state();
    drop(take_archive());
    assert!(!has_pending_archive());
    store_archive(ArchiveSource::Bytes(vec![1, 2, 3]));
    assert!(has_pending_archive());
    assert_eq!(take_archive(), Some(ArchiveSource::Bytes(vec![1, 2, 3])));
    assert!(!has_pending_archive());
    assert_eq!(take_archive(), None);
}

#[test]
fn store_and_take_archive_path_roundtrip() {
    let _guard = lock_state();
    let path = std::env::temp_dir().join(format!("cryptonote-egui-path-{}.cryptonote", std::process::id()));
    std::fs::write(&path, b"archive bytes").expect("write archive");
    store_archive(ArchiveSource::Path(path.clone()));
    assert!(has_pending_archive());
    let taken = take_archive().expect("archive present");
    assert_eq!(taken, ArchiveSource::Path(path.clone()));
    assert_eq!(taken.into_bytes().expect("read bytes"), b"archive bytes");
    assert!(!has_pending_archive());
    drop(std::fs::remove_file(path));
}

#[test]
fn take_archive_path_missing_file_returns_error() {
    let _guard = lock_state();
    drop(take_archive());
    let path = std::env::temp_dir().join("cryptonote-egui-missing.cryptonote");
    store_archive(ArchiveSource::Path(path));
    let taken = take_archive().expect("archive present");
    assert!(taken.into_bytes().is_err());
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn load_archive_opens_plain_package_from_stored_path() {
    let _guard = lock_state();
    drop(take_archive());
    let path = std::env::temp_dir().join(format!("cryptonote-egui-plain-{}.cryptonote", std::process::id()));
    let package =
        run_async(create_archive_package_async("deep link note", &[], "", None, |_| {})).expect("package created");
    std::fs::write(&path, package).expect("write package");
    store_archive(ArchiveSource::Path(path.clone()));
    let source = take_archive().expect("archive present");
    let opened = run_async(load_archive_async(source, |_| {})).expect("archive loaded");
    assert!(matches!(opened.screen, Screen::View));
    assert_eq!(opened.note, "deep link note");
    assert!(opened.attachments.is_empty());
    assert!(matches!(opened.external, External::Nothing));
    drop(std::fs::remove_file(path));
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn load_archive_routes_encrypted_package_to_open_screen() {
    let _guard = lock_state();
    drop(take_archive());
    let path = std::env::temp_dir().join(format!("cryptonote-egui-cipher-{}.cryptonote", std::process::id()));
    let package = run_async(create_archive_package_async(
        "secret",
        &[],
        "pw",
        Some(CipherType::Aes256Gcm),
        |_| {},
    ))
    .expect("package created");
    std::fs::write(&path, package).expect("write package");
    let source = ArchiveSource::Path(path.clone());
    let meta = read_archive_metadata(&source).expect("metadata");
    assert!(meta.cipher.is_some());
    let opened = run_async(load_archive_async(source, |_| {})).expect("archive loaded");
    assert!(matches!(opened.screen, Screen::Open));
    assert!(matches!(opened.external, External::Archive(_)));
    drop(std::fs::remove_file(path));
}
