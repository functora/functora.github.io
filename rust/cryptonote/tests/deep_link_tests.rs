use cryptonote::archive::ArchiveSource;
use cryptonote::{set_schedule_update, store_archive, store_url, take_archive, take_url, url_to_route};
use std::sync::{Arc, Mutex, MutexGuard};

static STATE_LOCK: Mutex<()> = Mutex::new(());

fn lock_state() -> MutexGuard<'static, ()> {
    STATE_LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

#[test]
fn url_to_route_with_query_returns_formatted() {
    let result = url_to_route("https://example.com/?note=abc");
    assert_eq!(result, Some("/?note=abc".to_string()));
}

#[test]
fn url_to_route_multiple_params() {
    let result = url_to_route("https://example.com/?screen=view&note=abc123");
    assert_eq!(result, Some("/?screen=view&note=abc123".to_string()));
}

#[test]
fn url_to_route_no_query_returns_none() {
    let result = url_to_route("https://example.com/");
    assert_eq!(result, None);
}

#[test]
fn url_to_route_no_query_string_returns_none() {
    let result = url_to_route("no-question-mark");
    assert_eq!(result, None);
}

#[test]
fn url_to_route_empty_query_returns_empty() {
    let result = url_to_route("https://example.com/?");
    assert_eq!(result, Some("/?".to_string()));
}

#[test]
fn url_to_route_only_hash() {
    let result = url_to_route("https://example.com/#section");
    assert_eq!(result, None);
}

#[test]
fn store_and_take_url_roundtrip() {
    let _guard = lock_state();
    store_url("https://test.com/?note=xyz".into());
    let taken = take_url();
    assert_eq!(taken, Some("https://test.com/?note=xyz".to_string()));
    assert_eq!(take_url(), None);
}

#[test]
fn take_url_empty_when_nothing_stored() {
    let _guard = lock_state();
    let _ = take_url();
    assert_eq!(take_url(), None);
}

#[test]
fn store_url_overwrites_previous() {
    let _guard = lock_state();
    store_url("first".into());
    store_url("second".into());
    assert_eq!(take_url(), Some("second".to_string()));
}

#[test]
fn store_url_triggers_scheduled_update() {
    use std::sync::atomic::{AtomicBool, Ordering};
    let _guard = lock_state();
    let _ = take_url();
    let called = Arc::new(AtomicBool::new(false));
    let c = called.clone();
    set_schedule_update(Arc::new(move || {
        c.store(true, Ordering::SeqCst);
    }));
    store_url("trigger".into());
    assert!(called.load(Ordering::SeqCst));
    let _ = take_url();
    // clean up: replace with no-op
    set_schedule_update(Arc::new(|| {}));
}

#[test]
fn store_and_take_archive_roundtrip() {
    let _guard = lock_state();
    store_archive(ArchiveSource::Bytes(vec![1, 2, 3]));
    assert_eq!(take_archive(), Some(ArchiveSource::Bytes(vec![1, 2, 3])));
    assert_eq!(take_archive(), None);
}

#[test]
fn store_archive_empty_bytes() {
    let _guard = lock_state();
    store_archive(ArchiveSource::Bytes(Vec::new()));
    assert_eq!(take_archive(), Some(ArchiveSource::Bytes(Vec::new())));
}

#[test]
fn take_archive_empty_when_nothing_stored() {
    let _guard = lock_state();
    let _ = take_archive();
    assert_eq!(take_archive(), None);
}

#[test]
fn store_archive_overwrites_previous() {
    let _guard = lock_state();
    store_archive(ArchiveSource::Bytes(vec![1]));
    store_archive(ArchiveSource::Bytes(vec![2, 3]));
    assert_eq!(take_archive(), Some(ArchiveSource::Bytes(vec![2, 3])));
}

#[test]
fn store_archive_triggers_scheduled_update() {
    use std::sync::atomic::{AtomicBool, Ordering};
    let _guard = lock_state();
    let _ = take_archive();
    let called = Arc::new(AtomicBool::new(false));
    let c = called.clone();
    set_schedule_update(Arc::new(move || {
        c.store(true, Ordering::SeqCst);
    }));
    store_archive(ArchiveSource::Bytes(vec![4, 5, 6]));
    assert!(called.load(Ordering::SeqCst));
    let _ = take_archive();
    // clean up: replace with no-op
    set_schedule_update(Arc::new(|| {}));
}

#[test]
fn store_and_take_archive_path_roundtrip() {
    use std::io::Write;
    let _guard = lock_state();
    let path = std::env::temp_dir().join(format!("cryptonote-deeplink-{}.cryptonote", std::process::id()));
    std::fs::File::create(&path)
        .expect("create failed")
        .write_all(b"archive bytes")
        .expect("write failed");
    store_archive(ArchiveSource::Path(path.clone()));
    let taken = take_archive().expect("archive missing");
    assert_eq!(taken, ArchiveSource::Path(path.clone()));
    assert_eq!(take_archive(), None);
    assert_eq!(taken.into_bytes().unwrap(), b"archive bytes");
    std::fs::remove_file(path).ok();
}

#[test]
fn take_archive_path_missing_file_returns_bytes_error() {
    let _guard = lock_state();
    let path = std::env::temp_dir().join("cryptonote-deeplink-missing.cryptonote");
    store_archive(ArchiveSource::Path(path));
    let taken = take_archive().expect("archive missing");
    assert!(taken.into_bytes().is_err());
}
