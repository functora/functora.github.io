//! Sidebar divider visibility (root cause: the 1px divider was painted at
//! `frame.min.x + 0.5` while its clip edge jitters against the frame origin
//! with subpixel rounding as content width changes. On some widths the clip
//! consumes the line almost entirely, e.g. Russian labels hiding the
//! desktop sidebar's left border. The divider must keep clearance from the
//! actual clip edge on every content width.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

/// Renders the desktop shell nesting (right panel + scroll + sidebar) with
/// the given labels and returns `(vline_x, clip_min_x)` of the divider.
fn divider_geometry(labels: &[String]) -> Option<(f32, f32)> {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        time: Some(1.0 / 60.0),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let refs: Vec<&str> = labels.iter().map(String::as_str).collect();
        let effective = functora_egui::layout::shell::sidebar_effective_width(ui.ctx(), &refs);
        let panel_outer = effective + 16.0;
        let _ = egui::Panel::right("sidebar_panel")
            .exact_size(panel_outer)
            .frame(egui::Frame::NONE)
            .resizable(false)
            .show_separator_line(false)
            .show(ui, |panel_ui| {
                let mut collapsed = false;
                let _ = egui::ScrollArea::vertical().show(panel_ui, |scroll_ui| {
                    let _ = functora_egui::Sidebar::new()
                        .width(effective)
                        .collapsible()
                        .show(scroll_ui, &mut collapsed, |side| {
                            for label in labels {
                                let _ = functora_egui::Button::new(label.clone())
                                    .icon(functora_egui::LucideIcon::House)
                                    .variant(functora_egui::ButtonVariant::Ghost)
                                    .full_width()
                                    .show(side);
                            }
                        });
                });
            });
    });
    out.textures_delta.clear();
    out.shapes.iter().find_map(|clipped| match &clipped.shape {
        Shape::LineSegment { points, .. }
            if (points[0].x - points[1].x).abs() < 1.0
                && (points[0].y - points[1].y).abs() > 100.0 =>
        {
            Some((points[0].x, clipped.clip_rect.min.x))
        }
        _ => None,
    })
}

#[test]
fn divider_keeps_clearance_from_clip_on_every_width() {
    let mut cases: Vec<Vec<String>> = [5, 10, 15, 20, 25, 30, 35, 40]
        .iter()
        .map(|n| vec!["X".repeat(*n)])
        .collect();
    cases.push(
        [
            "Главная",
            "Открыть URL",
            "Условия использования",
            "Политика конфиденциальности",
            "Пожертвовать",
        ]
        .iter()
        .map(ToString::to_string)
        .collect(),
    );
    for labels in &cases {
        let (vline_x, clip_min_x) = divider_geometry(labels).expect("divider line must be painted");
        let margin = vline_x - clip_min_x;
        assert!(
            (0.75..=1.5).contains(&margin),
            "divider must keep ~1px clearance from the clip edge, got margin {margin:.2} for {labels:?}"
        );
    }
}
