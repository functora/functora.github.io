//! Shared egui kittest harness: drives a headless context frame by frame
//! and collects what was painted, so widget tests assert on geometry
//! instead of mirroring layout internals.

use egui::{Pos2, RawInput, Rect, Shape, Vec2};

/// Drives one headless context across frames of a fixed screen size.
pub struct Harness {
    pub ctx: egui::Context,
    screen: Vec2,
    frame: u32,
}

impl Harness {
    #[must_use]
    pub fn new(screen: Vec2) -> Self {
        Self {
            ctx: egui::Context::default(),
            screen,
            frame: 0,
        }
    }

    /// Runs one frame of `body` and returns the raw egui output.
    pub fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, self.screen)),
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

/// Recursively collects every painted text galley with its screen rect.
pub fn collect_texts(shape: &Shape, out: &mut Vec<(Rect, String)>) {
    match shape {
        Shape::Text(text) => out.push((
            text.galley.rect.translate(text.pos.to_vec2()),
            text.galley.text().to_owned(),
        )),
        Shape::Vec(shapes) => {
            for inner in shapes {
                collect_texts(inner, out);
            }
        }
        _ => {}
    }
}

/// Every painted text and its rect, in paint order.
#[must_use]
pub fn all_texts(out: &egui::FullOutput) -> Vec<(Rect, String)> {
    let mut found = Vec::new();
    for clipped in &out.shapes {
        collect_texts(&clipped.shape, &mut found);
    }
    found
}

/// Solid widget boxes in the 16..=64px height band, e.g. buttons and tabs.
#[must_use]
pub fn box_rects(out: &egui::FullOutput) -> Vec<Rect> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect_shape)
                if rect_shape.fill.a() == 255
                    && (16.0..=64.0).contains(&rect_shape.rect.height())
                    && rect_shape.rect.width() >= 8.0 =>
            {
                Some(rect_shape.rect)
            }
            _ => None,
        })
        .collect()
}

/// Panics unless `inner` lies fully inside `outer`.
///
/// # Panics
/// When `inner` escapes `outer`.
pub fn assert_inside(inner: Rect, outer: Rect, what: &str) {
    assert!(
        outer.contains(inner.min) && outer.contains(inner.max),
        "{what} at {inner:?} escapes its container {outer:?}"
    );
}

/// Panics unless `inner` lies horizontally inside `outer`.
///
/// # Panics
/// When `inner` escapes `outer` horizontally.
pub fn assert_inside_x(inner: Rect, outer: Rect, what: &str) {
    assert!(
        outer.min.x <= inner.min.x && inner.max.x <= outer.max.x,
        "{what} at {inner:?} escapes its container horizontally {outer:?}"
    );
}
