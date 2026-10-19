#![allow(clippy::unwrap_used, clippy::expect_used)]

use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::state::ActionMode;
use functora_egui::paste_image::{self, PasteImage};

const PIXEL_PNG: &[u8] = &[
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 6, 0, 0, 0, 31, 21, 196,
    137, 0, 0, 0, 10, 73, 68, 65, 84, 120, 156, 99, 0, 1, 0, 0, 5, 0, 1, 13, 10, 45, 180, 0, 0, 0, 0, 73, 69, 78, 68,
    174, 66, 96, 130,
];

fn slot_guard() -> std::sync::MutexGuard<'static, ()> {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    LOCK.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn drain_slot(app: &mut CryptonoteApp) {
    let ctx = egui::Context::default();
    app.handle_paste_image(&ctx);
}

#[test]
fn slot_image_attaches() {
    let _guard = slot_guard();
    let mut app = CryptonoteApp::default();
    let toasts_before = app.toast.next_id();
    paste_image::store(PasteImage {
        name: "pasted-7.png".to_owned(),
        data: PIXEL_PNG.to_vec(),
    });
    drain_slot(&mut app);
    assert_eq!(app.temporary.attachments.len(), 1);
    assert_eq!(app.temporary.attachments[0].name, "pasted-7.png");
    assert_eq!(app.temporary.attachments[0].data.as_ref(), PIXEL_PNG);
    assert_eq!(app.toast.next_id(), toasts_before + 1);
}

#[test]
fn same_name_paste_replaces_previous() {
    let _guard = slot_guard();
    let mut app = CryptonoteApp::default();
    paste_image::store(PasteImage {
        name: "pasted-7.png".to_owned(),
        data: PIXEL_PNG.to_vec(),
    });
    drain_slot(&mut app);
    paste_image::store(PasteImage {
        name: "pasted-7.png".to_owned(),
        data: b"second".to_vec(),
    });
    drain_slot(&mut app);
    assert_eq!(app.temporary.attachments.len(), 1);
    assert_eq!(app.temporary.attachments[0].data.as_ref(), b"second");
}

#[test]
fn empty_slot_is_noop() {
    let _guard = slot_guard();
    let mut app = CryptonoteApp::default();
    let toasts_before = app.toast.next_id();
    drain_slot(&mut app);
    assert!(app.temporary.attachments.is_empty());
    assert_eq!(app.toast.next_id(), toasts_before);
}

#[test]
fn probe_not_spawned_off_create_tab() {
    let mut app = CryptonoteApp::default();
    app.temporary.action = ActionMode::Open;
    let ctx = egui::Context::default();
    ctx.input_mut(|i| {
        i.events.push(egui::Event::Key {
            key: egui::Key::V,
            physical_key: None,
            pressed: false,
            repeat: false,
            modifiers: egui::Modifiers {
                command: true,
                ..Default::default()
            },
        });
    });
    app.handle_paste_image(&ctx);
    assert!(app.paste_image_rx.is_none());
}

#[test]
fn probe_not_spawned_without_intent() {
    let mut app = CryptonoteApp::default();
    let ctx = egui::Context::default();
    app.handle_paste_image(&ctx);
    assert!(app.paste_image_rx.is_none());
}
