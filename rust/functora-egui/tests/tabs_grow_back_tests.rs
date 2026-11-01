//! Tab strips must recover their natural height after the window is widened:
//! the settled strip height depends only on the current width, never on how
//! the window was resized to get there.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::TabsValue;

fn labels() -> Vec<(usize, String)> {
    ["Создать заметку", "Открыть заметку", "Сканировать заметку"]
        .iter()
        .copied()
        .enumerate()
        .map(|(value, label)| (value, label.to_owned()))
        .collect()
}

/// Height of the tab strip (row plus trailing gap) on the last frame of the
/// given width plan.
///
/// # Panics
/// When the tab strip fails to report a height.
#[must_use]
fn settled_strip_height(plan: &[(f32, usize)]) -> f32 {
    let ctx = Context::default();
    let entries = labels();
    let mut selected = 0usize;
    let mut frame = 0u32;
    let mut strip_height = 0.0;
    for (width, frames) in plan {
        for _ in 0..*frames {
            frame += 1;
            let raw = RawInput {
                screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(*width, 640.0))),
                time: Some(f64::from(frame) / 60.0),
                ..Default::default()
            };
            let mut out = ctx.run_ui(raw, |ui| {
                let response =
                    TabsValue::new(&entries)
                        .fill_width()
                        .show(ui, &mut selected, |_, _| {});
                strip_height = response.rect.height();
            });
            out.textures_delta.clear();
        }
    }
    strip_height
}

/// Panics unless the two strip heights agree.
///
/// # Panics
/// When `via_resize` does not settle onto `natural`.
pub fn assert_recovers(what: &str, via_resize: f32, natural: f32) {
    assert!(
        (via_resize - natural).abs() < 0.5,
        "{what}: strip height {via_resize:.2} after resize history must equal the natural \
         {natural:.2} for the same final width"
    );
}

#[test]
fn tab_strip_recovers_natural_height_after_widening() {
    for final_width in [400.0, 480.0, 560.0, 720.0, 900.0, 1280.0] {
        let natural = settled_strip_height(&[(final_width, 8)]);
        let via_resize = settled_strip_height(&[(1280.0, 6), (320.0, 8), (final_width, 8)]);
        assert_recovers("widen from a wrapped narrow strip", via_resize, natural);
    }
}

#[test]
fn tab_strip_recovers_natural_height_after_shrinking_and_widening() {
    for final_width in [400.0, 480.0, 560.0, 720.0, 900.0, 1280.0] {
        let natural = settled_strip_height(&[(final_width, 8)]);
        let via_shrink = settled_strip_height(&[
            (1280.0, 6),
            (320.0, 8),
            (final_width, 8),
            (1280.0, 8),
            (final_width, 8),
        ]);
        assert_recovers(
            "round trip through a wrapped narrow strip",
            via_shrink,
            natural,
        );
    }
}
