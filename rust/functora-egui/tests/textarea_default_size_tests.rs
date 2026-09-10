#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Default textarea size: plain `Textarea` and `TextareaPasteClear` must be
//! tall enough to edit multi-line text without constant scrolling.

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);
const EXPECTED_MIN_HEIGHT: f32 = 192.0;

fn run_once(ctx: &Context, frame: u32, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        time: Some(f64::from(frame) / 60.0),
        events: Vec::<Event>::new(),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner_ui| body(inner_ui));
    });
    out.textures_delta.clear();
    out
}

fn border_heights(output: &egui::FullOutput) -> Vec<f32> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect) if rect.stroke.width > 0.0 => Some(rect.rect.height()),
            _ => None,
        })
        .collect()
}

#[test]
fn textarea_default_height_is_roomy() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let mut text = String::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::Textarea::new(&mut text).show(ui);
    };
    let out = run_once(&ctx, 2, &mut body);
    let heights = border_heights(&out);
    assert_eq!(heights.len(), 1, "expected one textarea border box");
    assert!(
        heights[0] >= EXPECTED_MIN_HEIGHT - 1.0,
        "default Textarea height must be at least {EXPECTED_MIN_HEIGHT}, got {}",
        heights[0],
    );
}

#[test]
fn textarea_paste_clear_default_height_is_roomy() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let mut text = String::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut text).show(ui);
    };
    let out = run_once(&ctx, 2, &mut body);
    let heights = border_heights(&out);
    let tallest = heights
        .iter()
        .copied()
        .reduce(f32::max)
        .expect("expected at least one border box");
    assert!(
        tallest >= EXPECTED_MIN_HEIGHT - 1.0,
        "default TextareaPasteClear height must be at least {EXPECTED_MIN_HEIGHT}, got {tallest} ({heights:?})",
    );
}
