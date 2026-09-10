//! Error-state regression: a failed image falls back to a 24 px box, and
//! egui truncates the alt text to that box with an ellipsis. The demo must
//! pin an exact size so the full alt text stays readable.

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

fn error_text_elided(out: &egui::FullOutput) -> Vec<bool> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) if text.galley.text().contains("checkerboard") => {
                Some(text.galley.elided)
            }
            _ => None,
        })
        .collect()
}

#[test]
fn error_image_alt_text_renders_untruncated() {
    let mut harness = Harness::new();
    // Mirrors the Error state demo: corrupt bytes with descriptive alt text
    // in an exact-size box with aspect maintenance off (failed loads fall
    // back to a square 24 px box otherwise).
    let mut body = |ui: &mut egui::Ui| {
        _ = ui.add(
            egui::Image::from_bytes("bytes://missing.png", b"corrupt".to_vec())
                .maintain_aspect_ratio(false)
                .fit_to_exact_size(egui::vec2(300.0, 64.0))
                .alt_text("Blue checkerboard, red circle"),
        );
    };
    let mut states = Vec::new();
    for _ in 0..5 {
        let out = harness.step(&mut body);
        states = error_text_elided(&out);
    }
    assert_eq!(
        states.len(),
        1,
        "error fallback text must render, got {states:?}",
    );
    assert!(
        !states[0],
        "error alt text must render in full instead of an ellipsis",
    );
}
