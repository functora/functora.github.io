//! `load_state`/`persist_value` round-trip with save/restore so the
//! shared storage file keeps its prior content.
#![cfg(feature = "storage")]

const KEY: &str = "functora_egui_test_roundtrip_key";

#[test]
fn persist_and_load_roundtrip() {
    use functora_egui::storage::{load_state, persist_value};

    let prior: Option<String> = load_state(KEY);
    persist_value(KEY, &"roundtrip-value".to_string());
    let loaded: Option<String> = load_state(KEY);
    assert_eq!(loaded.as_deref(), Some("roundtrip-value"));
    match &prior {
        Some(value) => persist_value(KEY, value),
        None => persist_value(KEY, &String::new()),
    }
    let restored: Option<String> = load_state(KEY);
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
    let loaded: Option<String> =
        functora_egui::storage::load_state("functora_egui_test_missing_key_xyz");
    assert_eq!(loaded, None);
}

#[test]
fn persistent_wrapper_get_set_update() {
    use functora_egui::storage::Persistent;

    let mut stored = Persistent::new("functora_egui_test_wrapper_key", 1u32);
    stored.set(2);
    assert_eq!(*stored.get(), 2);
    stored.update(|v| *v += 1);
    assert_eq!(*stored.get(), 3);
    assert_eq!(stored.into_inner(), 3);
}
