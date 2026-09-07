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

    fn step(
        &mut self,
        width: f32,
        height: f32,
        body: &mut dyn FnMut(&mut egui::Ui),
    ) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(width, height))),
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

fn text_positions(out: &egui::FullOutput, needle: &str) -> Vec<egui::Pos2> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) if text.galley.text() == needle => Some(text.pos),
            _ => None,
        })
        .collect()
}

fn frame_width(out: &egui::FullOutput, needle: &str) -> f32 {
    let positions = text_positions(out, needle);
    assert_eq!(positions.len(), 1, "expected one {needle} text");
    let pos = positions[0];
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect)
                if rect.rect.contains(pos) && (20.0..60.0).contains(&rect.rect.height()) =>
            {
                Some(rect.rect.width())
            }
            _ => None,
        })
        .fold(f32::INFINITY, f32::min)
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
    let out = app.step(1280.0, 4000.0, &mut body);
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
    for placeholder in ["Month", "Year"] {
        let width = frame_width(&out, placeholder);
        assert!(
            width >= 400.0,
            "{placeholder} select must fill its half, got width {width}"
        );
    }
}

#[test]
fn expiry_selects_stack_full_width_on_mobile() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        state.demo_field_group(ui);
    };
    let out = app.step(390.0, 2000.0, &mut body);
    for placeholder in ["Month", "Year"] {
        let width = frame_width(&out, placeholder);
        assert!(
            width >= 250.0,
            "{placeholder} select must stack full width on mobile, got width {width}"
        );
    }
}
