//! `Storage::load`/`Storage::persist` round-trip with save/restore so the
//! per-app storage file keeps its prior content.
#![cfg(feature = "storage")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use functora_egui::storage::Storage;

const APP: &str = "functora-egui-test";
const KEY: &str = "functora_egui_test_roundtrip_key";

fn storage() -> Storage {
    Storage::new(APP)
}

#[test]
fn persist_and_load_roundtrip() {
    let prior: Option<String> = storage().load(KEY);
    storage().persist(KEY, &"roundtrip-value".to_string());
    let loaded: Option<String> = storage().load(KEY);
    assert_eq!(loaded.as_deref(), Some("roundtrip-value"));
    match &prior {
        Some(value) => storage().persist(KEY, value),
        None => storage().persist(KEY, &String::new()),
    }
    let restored: Option<String> = storage().load(KEY);
    assert_eq!(
        restored,
        match &prior {
            Some(value) => Some(value.clone()),
            None => Some(String::new()),
        }
    );
}

#[test]
fn missing_key_loads_none() {
    let loaded: Option<String> = storage().load("functora_egui_test_missing_key_xyz");
    assert_eq!(loaded, None);
}

#[test]
fn persistent_wrapper_get_set_update() {
    let mut stored = storage().persistent("functora_egui_test_wrapper_key", 1u32);
    stored.set(2);
    assert_eq!(*stored.get(), 2);
    stored.update(|v| *v += 1);
    assert_eq!(*stored.get(), 3);
    assert_eq!(stored.into_inner(), 3);
}

#[test]
fn scopes_isolate_same_key() {
    let first = Storage::new("functora-egui-test-first");
    let second = Storage::new("functora-egui-test-second");
    first.persist(KEY, &"first-value".to_string());
    second.persist(KEY, &"second-value".to_string());
    let first_loaded: Option<String> = first.load(KEY);
    let second_loaded: Option<String> = second.load(KEY);
    assert_eq!(first_loaded.as_deref(), Some("first-value"));
    assert_eq!(second_loaded.as_deref(), Some("second-value"));
    let first_dir = first.files_dir().expect("first files dir");
    let second_dir = second.files_dir().expect("second files dir");
    assert_ne!(first_dir, second_dir);
}
