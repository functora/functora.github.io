//! Form screens regression: field labels must share their section's left edge
//! instead of centering inside flex columns.

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

fn assert_left_aligned(out: &egui::FullOutput, anchor: &str, needles: &[&str]) {
    let base = text_xs(out, anchor);
    assert_eq!(base.len(), 1, "{anchor} must render once");
    for needle in needles {
        let xs = text_xs(out, needle);
        assert_eq!(xs.len(), 1, "expected one {needle} label");
        assert!(
            (xs[0] - base[0]).abs() < 1.0,
            "{needle} at x={} must align with {anchor} at x={}",
            xs[0],
            base[0]
        );
    }
}

#[test]
fn form_labels_share_left_edge() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        state.demo_field_group(ui);
        state.demo_field_set(ui);
        state.demo_field_description(ui);
    };
    let out = app.step(4000.0, &mut body);
    assert_left_aligned(
        &out,
        "Groups related fields with a legend and description.",
        &["Card number", "Expiry", "CVV"],
    );
    assert_left_aligned(
        &out,
        "A bordered fieldset container for grouped controls.",
        &["Full name", "Email"],
    );
    assert_left_aligned(&out, "Supporting helper text under a field.", &["Password"]);
}
