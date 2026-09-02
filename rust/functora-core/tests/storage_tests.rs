use functora_core::Error;
use functora_core::storage::{
    find_or_init_key, get_json_value, read_json_object, set_json_value, update_key,
};
use std::fmt::Debug;
use std::fs::{read_to_string, write};
use tempfile::TempDir;

fn ok<T, E>(result: Result<T, E>) -> T
where
    E: Debug,
{
    match result {
        Ok(value) => value,
        Err(error) => panic!("expected Ok, got: {error:?}"),
    }
}

fn err<T, E>(result: Result<T, E>) -> E
where
    T: Debug,
{
    match result {
        Err(error) => error,
        Ok(value) => panic!("expected Err, got: {value:?}"),
    }
}

fn some<T>(option: Option<T>) -> T {
    match option {
        Some(value) => value,
        None => panic!("expected Some"),
    }
}

fn temp_file(content: &str) -> std::io::Result<(TempDir, std::path::PathBuf)> {
    let dir = TempDir::new()?;
    let path = dir.path().join("storage.json");
    write(&path, content)?;
    Ok((dir, path))
}

#[test]
fn update_key_inserts_new_key() {
    let (_dir, path) = ok(temp_file("{}"));
    ok(update_key(&path, "theme", "dark"));
    let content = ok(read_to_string(&path));
    assert!(content.contains(r#""theme""#));
    assert!(content.contains(r#""dark""#));
}

#[test]
fn update_key_updates_existing_key() {
    let (_dir, path) = ok(temp_file(r#"{"theme":"light"}"#));
    ok(update_key(&path, "theme", "dark"));
    let content = ok(read_to_string(&path));
    assert!(content.contains(r#""dark""#));
    assert!(!content.contains(r#""light""#));
}

#[test]
fn update_key_preserves_other_keys() {
    let (_dir, path) = ok(temp_file(r#"{"other":"value"}"#));
    ok(update_key(&path, "theme", "dark"));
    let content = ok(read_to_string(&path));
    assert!(content.contains(r#""other""#));
    assert!(content.contains(r#""value""#));
    assert!(content.contains(r#""theme""#));
}

#[test]
fn update_key_returns_error_for_invalid_json() {
    let (_dir, path) = ok(temp_file("not json"));
    let result = update_key(&path, "key", "val");
    assert!(result.is_err());
}

#[test]
fn update_key_returns_error_for_non_object_json() {
    let (_dir, path) = ok(temp_file("[]"));
    let result = update_key(&path, "key", "val");
    assert!(result.is_err());
    assert!(matches!(err(result), Error::NotJsonObject(_)));
}

#[test]
fn find_or_init_key_returns_existing_value() {
    let (_dir, path) = ok(temp_file(r#"{"theme":"dark"}"#));
    let val: String = ok(find_or_init_key(&path, "theme", || "light".to_string()));
    assert_eq!(val, "dark");
}

#[test]
fn find_or_init_key_returns_default_when_missing() {
    let (_dir, path) = ok(temp_file("{}"));
    let val: String = ok(find_or_init_key(&path, "theme", || "light".to_string()));
    assert_eq!(val, "light");
}

#[test]
fn find_or_init_key_initializes_missing_key() {
    let (_dir, path) = ok(temp_file("{}"));
    let _ = ok(find_or_init_key::<_, String, _>(&path, "theme", || {
        "light".to_string()
    }));
    let content = ok(read_to_string(&path));
    assert!(content.contains(r#""theme""#));
    assert!(content.contains(r#""light""#));
}

#[test]
fn find_or_init_key_preserves_init_fn_order() {
    let (_dir, path) = ok(temp_file("{}"));
    let mut call_count = 0;
    let val: i32 = ok(find_or_init_key(&path, "count", || {
        call_count += 1;
        42
    }));
    assert_eq!(val, 42);
    assert_eq!(call_count, 1);

    let val2: i32 = ok(find_or_init_key(&path, "count", || {
        call_count += 1;
        99
    }));
    assert_eq!(val2, 42);
    assert_eq!(call_count, 1);
}

#[test]
fn find_or_init_key_complex_types() {
    let (_dir, path) = ok(temp_file(r#"{"count":42}"#));
    let val: i32 = ok(find_or_init_key(&path, "count", || 0));
    assert_eq!(val, 42);
}

#[test]
fn read_json_object_parses_valid_json() {
    let (_dir, path) = ok(temp_file(r#"{"a":1,"b":"test"}"#));
    let json = ok(read_json_object(&path));
    assert_eq!(some(json["a"].as_i64()), 1);
    assert_eq!(some(json["b"].as_str()), "test");
}

#[test]
fn read_json_object_handles_empty_object() {
    let (_dir, path) = ok(temp_file("{}"));
    let json = ok(read_json_object(&path));
    assert!(json.is_object());
    assert!(some(json.as_object()).is_empty());
}

#[test]
fn get_json_value_returns_value_when_present() {
    let (_dir, path) = ok(temp_file(r#"{"theme":"dark"}"#));
    let val = ok(get_json_value(&path, "theme"));
    assert!(val.is_some());
    assert_eq!(some(some(val).as_str()), "dark");
}

#[test]
fn get_json_value_returns_none_when_missing() {
    let (_dir, path) = ok(temp_file(r#"{"other":"value"}"#));
    let val = ok(get_json_value(&path, "missing"));
    assert!(val.is_none());
}

#[test]
fn set_json_value_inserts_new_key() {
    let (_dir, path) = ok(temp_file("{}"));
    ok(set_json_value(&path, "theme", "dark"));
    let json = ok(read_json_object(&path));
    assert_eq!(some(json["theme"].as_str()), "dark");
}

#[test]
fn set_json_value_updates_existing_key() {
    let (_dir, path) = ok(temp_file(r#"{"theme":"light"}"#));
    ok(set_json_value(&path, "theme", "dark"));
    let json = ok(read_json_object(&path));
    assert_eq!(some(json["theme"].as_str()), "dark");
}

#[test]
fn set_json_value_preserves_other_keys() {
    let (_dir, path) = ok(temp_file(r#"{"existing":"value"}"#));
    ok(set_json_value(&path, "new", "data"));
    let json = ok(read_json_object(&path));
    assert_eq!(some(json["existing"].as_str()), "value");
    assert_eq!(some(json["new"].as_str()), "data");
}

#[test]
fn concurrent_update_key_preserves_all_keys() {
    let (_dir, path) = ok(temp_file("{}"));
    let mut threads = Vec::new();
    for i in 0..8 {
        let thread_path = path.clone();
        threads.push(std::thread::spawn(move || {
            for _ in 0..25 {
                ok(update_key(&thread_path, &format!("key{i}"), i));
            }
        }));
    }
    for thread in threads {
        ok(thread.join());
    }
    let json = ok(read_json_object(&path));
    for i in 0..8 {
        let value = some(json[format!("key{i}")].as_i64());
        assert_eq!(value, i64::from(i));
    }
}
