//! Platform screens regression: clipboard labels share the description's left
//! edge, and picked file rows share one left edge inside preview cards.

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

fn text_xs_prefix(out: &egui::FullOutput, prefix: &str) -> Vec<f32> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) if text.galley.text().starts_with(prefix) => Some(text.pos.x),
            _ => None,
        })
        .collect()
}

#[test]
fn clipboard_labels_share_left_edge() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| state.demo_clipboard(ui);
    let out = app.step(2500.0, &mut body);
    let anchor = text_xs(
        &out,
        "Clipboard read/write via arboard (desktop), navigator.clipboard (web), ClipboardManager (Android).",
    );
    assert_eq!(anchor.len(), 1, "description must render once");
    for needle in ["Write to clipboard", "Last pasted"] {
        let xs = text_xs(&out, needle);
        assert_eq!(xs.len(), 1, "expected one {needle} label");
        assert!(
            (xs[0] - anchor[0]).abs() < 1.0,
            "{needle} at x={} must align with description at x={}",
            xs[0],
            anchor[0]
        );
    }
}

#[test]
fn picked_file_rows_share_left_edge() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    state.platform.picked = vec![
        ("a.txt".to_owned(), b"hello".to_vec()),
        ("much-longer-name.txt".to_owned(), b"hello world".to_vec()),
    ];
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| state.demo_files(ui);
    let out = app.step(2500.0, &mut body);
    let mut edges = ["a.txt (", "much-longer-name.txt ("]
        .iter()
        .map(|prefix| {
            let xs = text_xs_prefix(&out, prefix);
            assert_eq!(xs.len(), 1, "expected one row starting with {prefix}");
            xs[0]
        })
        .collect::<Vec<_>>();
    edges.sort_by(f32::total_cmp);
    assert!(
        edges[1] - edges[0] < 1.0,
        "picked file rows must share one left edge, got {edges:?}"
    );
}
