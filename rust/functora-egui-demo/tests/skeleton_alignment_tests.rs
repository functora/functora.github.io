//! Skeleton demo regression: placeholder bars must keep their intrinsic
//! 200/180/120 lengths and share one left edge instead of stretching full
//! width inside their flex column.

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

fn bar_geometries(out: &egui::FullOutput) -> Vec<egui::Rect> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Rect(rect) if (rect.rect.height() - 16.0).abs() < 1.0 => Some(rect.rect),
            _ => None,
        })
        .collect()
}

#[test]
fn skeleton_bars_keep_intrinsic_lengths() {
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        functora_egui_demo::ShowcaseApp::demo_skeleton(ui);
    };
    let _ = app.step(800.0, &mut body);
    let out = app.step(800.0, &mut body);
    let bars = bar_geometries(&out);
    assert!(!bars.is_empty(), "skeleton bars must render");
    assert!(
        bars.iter().all(|rect| rect.width() <= 300.0),
        "bars must keep their 200/180/120 lengths instead of stretching full width: {:?}",
        bars.iter().map(egui::Rect::width).collect::<Vec<_>>()
    );
    for width in [200.0, 180.0, 120.0] {
        assert!(
            bars.iter().any(|rect| (rect.width() - width).abs() < 2.0),
            "expected a {width}px bar, got {:?}",
            bars.iter().map(egui::Rect::width).collect::<Vec<_>>()
        );
    }
    let mut edges = bars.iter().map(|rect| rect.min.x).collect::<Vec<_>>();
    edges.sort_by(f32::total_cmp);
    assert!(
        edges[edges.len() - 1] - edges[0] < 1.0,
        "bars must share one left edge, got {edges:?}"
    );
}
