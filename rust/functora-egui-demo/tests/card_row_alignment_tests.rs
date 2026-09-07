//! Responsive info cards regression: rows inside the `Breakpoint`, `Spacing`,
//! and `TouchTarget` cards must share one left edge instead of centering.

use egui::{Context, Pos2, RawInput, Rect, Vec2};

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

fn text_xs_matching(out: &egui::FullOutput, matches: &dyn Fn(&str) -> bool) -> Vec<f32> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) if matches(text.galley.text()) => Some(text.pos.x),
            _ => None,
        })
        .collect()
}

fn assert_shared_left_edge(out: &egui::FullOutput, group: &str, count: usize) {
    let mut edges = match group {
        "breakpoint" => text_xs_matching(out, &|text| {
            text.starts_with("Breakpoint: ")
                || text == "mobile"
                || text == "desktop"
                || text.starts_with("Spacing: ")
        }),
        "spacing" => text_xs_matching(out, &|text| {
            matches!(
                text,
                "touch_height" | "touch_padding" | "gap" | "page_padding" | "content_max_width"
            )
        }),
        _ => text_xs_matching(out, &|text| {
            text.starts_with("Touch target height:") || text.starts_with("Touch padding:")
        }),
    };
    assert_eq!(edges.len(), count, "{group} rows must all render");
    edges.sort_by(f32::total_cmp);
    assert!(
        edges[edges.len() - 1] - edges[0] < 1.0,
        "{group} rows must share one left edge, got {edges:?}"
    );
}

#[test]
fn info_card_rows_share_left_edge() {
    let mut state = functora_egui_demo::ShowcaseApp::default();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        functora_egui_demo::ShowcaseApp::demo_breakpoint(ui);
        functora_egui_demo::ShowcaseApp::demo_spacing(ui);
        state.demo_touch_target(ui);
    };
    let out = app.step(2500.0, &mut body);
    assert_shared_left_edge(&out, "breakpoint", 3);
    assert_shared_left_edge(&out, "spacing", 5);
    assert_shared_left_edge(&out, "touch", 2);
}
