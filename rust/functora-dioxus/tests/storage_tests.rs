use functora_dioxus::Error;
use functora_dioxus::storage::mobile::{
    find_or_init_key, get_json_value, read_json_object, set_json_value, update_key,
};
use std::fs::{read_to_string, write};
use tempfile::TempDir;

fn temp_file(content: &str) -> (TempDir, std::path::PathBuf) {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("storage.json");
    write(&path, content).unwrap();
    (dir, path)
}

#[test]
fn update_key_inserts_new_key() {
    let (_dir, path) = temp_file(r#"{}"#);
    update_key(&path, "theme", &"dark").unwrap();
    let content = read_to_string(&path).unwrap();
    assert!(content.contains(r#""theme""#));
    assert!(content.contains(r#""dark""#));
}

#[test]
fn update_key_updates_existing_key() {
    let (_dir, path) = temp_file(r#"{"theme":"light"}"#);
    update_key(&path, "theme", &"dark").unwrap();
    let content = read_to_string(&path).unwrap();
    assert!(content.contains(r#""dark""#));
    assert!(!content.contains(r#""light""#));
}

#[test]
fn update_key_preserves_other_keys() {
    let (_dir, path) = temp_file(r#"{"other":"value"}"#);
    update_key(&path, "theme", &"dark").unwrap();
    let content = read_to_string(&path).unwrap();
    assert!(content.contains(r#""other""#));
    assert!(content.contains(r#""value""#));
    assert!(content.contains(r#""theme""#));
}

#[test]
fn update_key_returns_error_for_invalid_json() {
    let (_dir, path) = temp_file("not json");
    let result = update_key(&path, "key", &"val");
    assert!(result.is_err());
}

#[test]
fn update_key_returns_error_for_non_object_json() {
    let (_dir, path) = temp_file(r#"[]"#);
    let result = update_key(&path, "key", &"val");
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), Error::NotJsonObject(_)));
}

#[test]
fn find_or_init_key_returns_existing_value() {
    let (_dir, path) = temp_file(r#"{"theme":"dark"}"#);
    let val: String = find_or_init_key(&path, "theme", || "light".to_string()).unwrap();
    assert_eq!(val, "dark");
}

#[test]
fn find_or_init_key_returns_default_when_missing() {
    let (_dir, path) = temp_file(r#"{}"#);
    let val: String = find_or_init_key(&path, "theme", || "light".to_string()).unwrap();
    assert_eq!(val, "light");
}

#[test]
fn find_or_init_key_initializes_missing_key() {
    let (_dir, path) = temp_file(r#"{}"#);
    let _ = find_or_init_key::<_, String, _>(&path, "theme", || "light".to_string()).unwrap();
    let content = read_to_string(&path).unwrap();
    assert!(content.contains(r#""theme""#));
    assert!(content.contains(r#""light""#));
}

#[test]
fn find_or_init_key_preserves_init_fn_order() {
    let (_dir, path) = temp_file(r#"{}"#);
    let mut call_count = 0;
    let val: i32 = find_or_init_key(&path, "count", || {
        call_count += 1;
        42
    })
    .unwrap();
    assert_eq!(val, 42);
    assert_eq!(call_count, 1);

    let val2: i32 = find_or_init_key(&path, "count", || {
        call_count += 1;
        99
    })
    .unwrap();
    assert_eq!(val2, 42);
    assert_eq!(call_count, 1);
}

#[test]
fn find_or_init_key_complex_types() {
    let (_dir, path) = temp_file(r#"{"count":42}"#);
    let val: i32 = find_or_init_key(&path, "count", || 0).unwrap();
    assert_eq!(val, 42);
}

#[test]
fn read_json_object_parses_valid_json() {
    let (_dir, path) = temp_file(r#"{"a":1,"b":"test"}"#);
    let json = read_json_object(&path).unwrap();
    assert_eq!(json["a"].as_i64().unwrap(), 1);
    assert_eq!(json["b"].as_str().unwrap(), "test");
}

#[test]
fn read_json_object_handles_empty_object() {
    let (_dir, path) = temp_file(r#"{}"#);
    let json = read_json_object(&path).unwrap();
    assert!(json.is_object());
    assert!(json.as_object().unwrap().is_empty());
}

#[test]
fn get_json_value_returns_value_when_present() {
    let (_dir, path) = temp_file(r#"{"theme":"dark"}"#);
    let val = get_json_value(&path, "theme").unwrap();
    assert!(val.is_some());
    assert_eq!(val.unwrap().as_str().unwrap(), "dark");
}

#[test]
fn get_json_value_returns_none_when_missing() {
    let (_dir, path) = temp_file(r#"{"other":"value"}"#);
    let val = get_json_value(&path, "missing").unwrap();
    assert!(val.is_none());
}

#[test]
fn set_json_value_inserts_new_key() {
    let (_dir, path) = temp_file(r#"{}"#);
    set_json_value(&path, "theme", &"dark").unwrap();
    let json = read_json_object(&path).unwrap();
    assert_eq!(json["theme"].as_str().unwrap(), "dark");
}

#[test]
fn set_json_value_updates_existing_key() {
    let (_dir, path) = temp_file(r#"{"theme":"light"}"#);
    set_json_value(&path, "theme", &"dark").unwrap();
    let json = read_json_object(&path).unwrap();
    assert_eq!(json["theme"].as_str().unwrap(), "dark");
}

#[test]
fn set_json_value_preserves_other_keys() {
    let (_dir, path) = temp_file(r#"{"existing":"value"}"#);
    set_json_value(&path, "new", &"data").unwrap();
    let json = read_json_object(&path).unwrap();
    assert_eq!(json["existing"].as_str().unwrap(), "value");
    assert_eq!(json["new"].as_str().unwrap(), "data");
}

#[test]
fn test_files_dir_not_fails_on_non_android_ios() {
    #[cfg(not(any(target_os = "android", target_os = "ios")))]
    {
        let result = functora_dioxus::storage::mobile::files_dir();
        assert!(result.is_ok());
        let path = result.unwrap();
        assert!(path.exists());
    }
}
