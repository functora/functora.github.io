//! Paste-clear unification (plan step 4): both widgets share one response
//! type and one clipboard/slot core, so a fix in the shared logic applies to
//! single-line and multi-line fields together.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use egui::{Context, Pos2, RawInput, Rect, Vec2};

/// `InputPasteClearResponse` and `TextareaPasteClearResponse` are two names
/// for the same shared response type: either widget's output must be usable
/// wherever the other is expected.
#[test]
fn both_paste_clear_responses_are_one_shared_type() {
    fn accept_input(_: functora_egui::InputPasteClearResponse) {}
    let ctx = Context::default();
    let mut single = "single".to_owned();
    let mut multi = "multi".to_owned();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
        time: Some(1.0 / 60.0),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            let _ = functora_egui::InputPasteClear::new(&mut single).show(inner);
            let multi_resp = functora_egui::TextareaPasteClear::new(&mut multi).show(inner);
            accept_input(multi_resp);
        });
    });
    out.textures_delta.clear();
    assert_eq!(single, "single");
    assert_eq!(multi, "multi");
}
