//! Regression tests for platform promise polling (plan step 2).
//!
//! `poll_platform_promises` must consume ready channels, restore pending ones,
//! and merge picked files without losing state.

use functora_egui_demo::ShowcaseApp;

fn ctx() -> egui::Context {
    egui::Context::default()
}

#[test]
fn ready_clipboard_read_sets_text_and_clears_slot() {
    let mut state = ShowcaseApp::default();
    let (tx, rx) = std::sync::mpsc::channel();
    state.platform.clipboard_rx = Some(rx);
    assert!(
        tx.send(Ok("hello".to_owned())).is_ok(),
        "test channel must send"
    );
    state.poll_platform_promises(&ctx());
    assert_eq!(state.platform.clipboard_read, "hello");
    assert!(state.platform.clipboard_rx.is_none());
}

#[test]
fn empty_clipboard_read_restores_slot() {
    let mut state = ShowcaseApp::default();
    let (_tx, rx) = std::sync::mpsc::channel::<Result<String, String>>();
    state.platform.clipboard_rx = Some(rx);
    state.poll_platform_promises(&ctx());
    assert!(state.platform.clipboard_rx.is_some());
}

#[test]
fn disconnected_clipboard_read_clears_slot() {
    let mut state = ShowcaseApp::default();
    let (tx, rx) = std::sync::mpsc::channel::<Result<String, String>>();
    state.platform.clipboard_rx = Some(rx);
    drop(tx);
    state.poll_platform_promises(&ctx());
    assert!(state.platform.clipboard_rx.is_none());
}

#[test]
fn ready_write_share_download_worker_clear_slots() {
    let mut state = ShowcaseApp::default();
    let (clip_tx, clip_rx) = std::sync::mpsc::channel();
    let (share_tx, share_rx) = std::sync::mpsc::channel();
    let (dl_tx, dl_rx) = std::sync::mpsc::channel();
    let (work_tx, work_rx) = std::sync::mpsc::channel();
    state.platform.clipboard_write_rx = Some(clip_rx);
    state.platform.share_rx = Some(share_rx);
    state.platform.download_rx = Some(dl_rx);
    state.platform.worker_rx = Some(work_rx);
    assert!(clip_tx.send(Ok(())).is_ok(), "test channel must send");
    assert!(share_tx.send(Ok(())).is_ok(), "test channel must send");
    assert!(
        dl_tx.send(Ok("hello.txt".to_owned())).is_ok(),
        "test channel must send"
    );
    assert!(
        work_tx.send(Ok("done".to_owned())).is_ok(),
        "test channel must send"
    );
    state.poll_platform_promises(&ctx());
    assert!(state.platform.clipboard_write_rx.is_none());
    assert!(state.platform.share_rx.is_none());
    assert!(state.platform.download_rx.is_none());
    assert!(state.platform.worker_rx.is_none());
}

#[test]
fn ready_pick_merges_files_and_clears_job_state() {
    let mut state = ShowcaseApp::default();
    state.platform.picked = vec![("old.txt".to_owned(), b"old".to_vec())];
    state.platform.pick_overlay_open = true;
    let (tx, rx) = std::sync::mpsc::channel();
    state.platform.pick_rx = Some(rx);
    assert!(
        tx.send(Ok(vec![
            ("old.txt".to_owned(), b"new".to_vec()),
            ("fresh.txt".to_owned(), b"data".to_vec()),
        ]))
        .is_ok(),
        "test channel must send"
    );
    state.poll_platform_promises(&ctx());
    assert!(state.platform.pick_rx.is_none());
    assert!(!state.platform.pick_overlay_open);
    assert!(state.platform.pick_cancel.is_none());
    assert!(state.platform.pick_job.is_none());
    assert!(state.platform.pick_progress.is_none());
    let names: Vec<&str> = state
        .platform
        .picked
        .iter()
        .map(|(name, _)| name.as_str())
        .collect();
    assert_eq!(names, vec!["old.txt", "fresh.txt"]);
    let refreshed = state
        .platform
        .picked
        .iter()
        .find(|(name, _)| name == "old.txt");
    assert!(refreshed.is_some(), "re-picked file must remain");
    if let Some((_, data)) = refreshed {
        assert_eq!(*data, b"new".to_vec());
    }
}
