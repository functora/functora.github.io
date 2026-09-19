#![allow(clippy::unwrap_used, clippy::expect_used)]

const APP_SRC: &str = include_str!("../src/app.rs");

#[test]
fn soft_keyboard_text_reaches_inputs_on_android() {
    assert!(
        APP_SRC.contains("poll_ime"),
        "update loop must poll the soft-keyboard state into egui events"
    );
    assert!(
        APP_SRC.contains("target_os = \"android\""),
        "ime polling must stay gated to android builds"
    );
}
