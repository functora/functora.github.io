use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::error::AppError;

#[test]
fn unseen_qr_error_dedups_repeats() {
    let mut app = CryptonoteApp::default();
    assert!(app.unseen_qr_error("boom"));
    assert!(!app.unseen_qr_error("boom"));
    assert!(app.unseen_qr_error("different"));
}

fn ctx() -> egui::Context {
    egui::Context::default()
}

fn send_pick(app: &mut CryptonoteApp, result: Result<Vec<(String, Vec<u8>)>, AppError>) {
    let (tx, rx) = std::sync::mpsc::channel();
    app.pick_rx = Some(rx);
    app.pick_overlay_open = true;
    assert!(tx.send(result).is_ok());
    app.poll_receivers(&ctx());
}

#[test]
fn pick_completion_adds_attachments_and_toasts() {
    let mut app = CryptonoteApp::default();
    let toasts_before = app.toast.next_id();
    send_pick(
        &mut app,
        Ok(vec![
            ("a.txt".to_owned(), b"hello".to_vec()),
            ("b.bin".to_owned(), vec![0u8, 1u8]),
        ]),
    );
    assert_eq!(app.temporary.attachments.len(), 2);
    assert!(app.pick_rx.is_none(), "slot must clear");
    assert!(!app.pick_overlay_open, "overlay must close");
    assert!(app.temporary.progress.is_none());
    assert_eq!(
        app.toast.next_id(),
        toasts_before + 1,
        "completion must fire exactly one toast"
    );
}

#[test]
fn pick_completion_dedups_by_name() {
    let mut app = CryptonoteApp::default();
    send_pick(&mut app, Ok(vec![("a.txt".to_owned(), b"old".to_vec())]));
    send_pick(&mut app, Ok(vec![("a.txt".to_owned(), b"new".to_vec())]));
    assert_eq!(app.temporary.attachments.len(), 1);
    let data = app.temporary.attachments[0].data.to_vec();
    assert_eq!(data, b"new".to_vec(), "re-pick must refresh bytes");
}

#[test]
fn pick_cancel_toasts_silently() {
    let mut app = CryptonoteApp::default();
    let toasts_before = app.toast.next_id();
    send_pick(&mut app, Err(AppError::Cancelled));
    assert!(app.pick_rx.is_none());
    assert!(!app.pick_overlay_open);
    assert_eq!(
        app.toast.next_id(),
        toasts_before + 1,
        "cancel must fire exactly one toast"
    );
}

#[test]
fn pick_error_toasts() {
    let mut app = CryptonoteApp::default();
    let toasts_before = app.toast.next_id();
    send_pick(&mut app, Err(AppError::NoFileSelected));
    assert!(app.pick_rx.is_none());
    assert!(!app.pick_overlay_open);
    assert_eq!(
        app.toast.next_id(),
        toasts_before + 1,
        "real errors must fire exactly one error toast"
    );
}
