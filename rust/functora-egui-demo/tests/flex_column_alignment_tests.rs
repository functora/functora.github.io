//! Flex column regression: nested form labels must left-align while inputs
//! keep spanning their column halves; badge column texts share one edge.

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

fn text_positions(out: &egui::FullOutput, needle: &str) -> Vec<egui::Pos2> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) if text.galley.text() == needle => Some(text.pos),
            _ => None,
        })
        .collect()
}

fn input_width(out: &egui::FullOutput, placeholder: &str) -> f32 {
    let positions = text_positions(out, placeholder);
    assert_eq!(positions.len(), 1, "expected one {placeholder} text");
    let pos = positions[0];
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Rect(rect)
                if rect.rect.contains(pos) && (20.0..60.0).contains(&rect.rect.height()) =>
            {
                Some(rect.rect.width())
            }
            _ => None,
        })
        .fold(f32::INFINITY, f32::min)
}

#[test]
fn nested_form_labels_share_column_left_edge() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| state.demo_flex(ui);
    let _ = app.step(9000.0, &mut body);
    let out = app.step(9000.0, &mut body);
    let anchor = text_xs(&out, "Nested flex: two-column form");
    assert_eq!(anchor.len(), 1, "section heading must render once");
    for needle in ["First Name", "Last Name"] {
        let xs = text_xs(&out, needle);
        assert_eq!(xs.len(), 1, "expected one {needle} label");
        assert!(
            (xs[0] - anchor[0]).abs() < 1.0,
            "{needle} at x={} must align with its column at x={}",
            xs[0],
            anchor[0]
        );
    }
    let mut right = ["Email", "Phone"]
        .iter()
        .map(|needle| {
            let xs = text_xs(&out, needle);
            assert_eq!(xs.len(), 1, "expected one {needle} label");
            xs[0]
        })
        .collect::<Vec<_>>();
    right.sort_by(f32::total_cmp);
    assert!(
        right[1] - right[0] < 1.0,
        "right column labels must share one left edge, got {right:?}"
    );
    for placeholder in ["John", "Doe", "john@example.com", "+1 555-1234"] {
        let width = input_width(&out, placeholder);
        assert!(
            width >= 400.0,
            "{placeholder} input must span its half, got width {width}"
        );
    }
}

#[test]
fn badge_column_texts_share_left_edge() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| state.demo_flex(ui);
    let _ = app.step(9000.0, &mut body);
    let out = app.step(9000.0, &mut body);
    let mut edges = ["First", "Second", "Third"]
        .iter()
        .map(|needle| {
            let xs = text_xs(&out, needle);
            assert_eq!(xs.len(), 1, "expected one {needle} badge");
            xs[0]
        })
        .collect::<Vec<_>>();
    edges.sort_by(f32::total_cmp);
    assert!(
        edges[2] - edges[0] < 1.0,
        "badge texts must share one left edge, got {edges:?}"
    );
}
