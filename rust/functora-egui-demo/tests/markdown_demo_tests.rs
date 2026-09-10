//! Markdown demo regression: the rendered preview card spans the full
//! content width like the Source textarea instead of shrinking to content.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui_demo::ShowcaseApp;

struct Harness {
    ctx: Context,
    frame: u32,
}

impl Harness {
    fn new() -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
        }
    }

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(f64::from(self.frame) / 60.0),
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }
}

#[test]
fn preview_card_spans_full_content_width() {
    let mut state = ShowcaseApp::default();
    state.platform.md_source = "Hi.".to_owned();
    let mut body = |ui: &mut egui::Ui| state.demo_markdown(ui);
    let out = Harness::new().step(&mut body);
    let mut narrow = Vec::new();
    for clipped in &out.shapes {
        if let Shape::Rect(rect) = &clipped.shape
            && rect.fill.a() > 0
            && rect.rect.width() > 30.0
            && rect.rect.width() < 1100.0
        {
            narrow.push(rect.rect);
        }
    }
    assert!(
        narrow.is_empty(),
        "all boxes must span full content width, found narrow boxes: {narrow:?}",
    );
}
