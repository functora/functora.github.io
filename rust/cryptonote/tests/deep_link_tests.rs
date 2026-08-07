#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote::archive::ArchiveSource;
use cryptonote::{set_schedule_update, store_archive, take_archive};
use std::sync::{Arc, Mutex, MutexGuard};

static STATE_LOCK: Mutex<()> = Mutex::new(());

fn lock_state() -> MutexGuard<'static, ()> {
    STATE_LOCK.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
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
    drop(std::fs::remove_file(path));
}

#[test]
fn take_archive_path_missing_file_returns_bytes_error() {
    let _guard = lock_state();
    let path = std::env::temp_dir().join("cryptonote-deeplink-missing.cryptonote");
    store_archive(ArchiveSource::Path(path));
    let taken = take_archive().expect("archive missing");
    assert!(taken.into_bytes().is_err());
}
