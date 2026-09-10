#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Textarea clipping: the inner scroll viewport must never extend past the
//! outer border box, or text rows paint outside the borders.

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);
const MD: &str = "# Hello\n\nThis is **bold**, *italic* and ~~struck~~.\n\n- item 1\n- item 2\n\n[link](https://example.com)\n\n| a | b |\n|---|---|\n| 1 | 2 |\n\n`code`";

fn border_box(output: &egui::FullOutput) -> Rect {
    let mut borders: Vec<Rect> = output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect) if rect.stroke.width > 0.0 => Some(rect.rect),
            _ => None,
        })
        .collect();
    assert_eq!(borders.len(), 1, "expected one border box");
    borders.pop().expect("border box")
}

#[test]
fn textarea_clip_stays_within_border() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let mut text = MD.to_owned();
    for frame in 0..3 {
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
            time: Some(f64::from(frame) / 60.0),
            events: Vec::<Event>::new(),
            ..Default::default()
        };
        let mut out = ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner_ui| {
                let _ = functora_egui::Textarea::new(&mut text)
                    .placeholder("# Hello")
                    .desired_width(inner_ui.available_width())
                    .show(inner_ui);
            });
        });
        out.textures_delta.clear();
        if frame == 2 {
            let border = border_box(&out);
            for clipped in &out.shapes {
                if let Shape::Text(shown) = &clipped.shape {
                    assert!(
                        clipped.clip_rect.bottom() <= border.bottom() + 1.0,
                        "scroll clip {:?} escapes border {:?} for {:?}",
                        clipped.clip_rect,
                        border,
                        shown.galley.text().chars().take(20).collect::<String>(),
                    );
                }
            }
        }
    }
}
