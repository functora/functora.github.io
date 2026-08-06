use functora_dioxus::deep_link::{set_schedule_update, store_url, take_url, url_to_route};
use std::sync::{Arc, Mutex, MutexGuard};

static STATE_LOCK: Mutex<()> = Mutex::new(());

fn lock_state() -> MutexGuard<'static, ()> {
    STATE_LOCK.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
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
    set_schedule_update(Arc::new(|| {}));
}
