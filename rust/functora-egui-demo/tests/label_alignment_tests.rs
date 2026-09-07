//! Label screen regression: demo labels must share the description's left
//! edge instead of centering inside their flex column.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

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

    fn step(&mut self, height: f32, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, height))),
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

fn text_xs(out: &egui::FullOutput, needle: &str) -> Vec<f32> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) if text.galley.text() == needle => Some(text.pos.x),
            _ => None,
        })
        .collect()
}

#[test]
fn labels_share_description_left_edge() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| state.demo_label(ui);
    let out = app.step(800.0, &mut body);
    let desc = text_xs(&out, "Labels pair with inputs in forms and settings.");
    assert_eq!(desc.len(), 1, "description must render once");
    for needle in ["Your email address", "Sizes", "Small label", "Muted label"] {
        let xs = text_xs(&out, needle);
        assert_eq!(xs.len(), 1, "expected one {needle} label");
        assert!(
            (xs[0] - desc[0]).abs() < 1.0,
            "{needle} at x={} must align with description at x={}",
            xs[0],
            desc[0]
        );
    }
}
