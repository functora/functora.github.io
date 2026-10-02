//! Sidebar divider alignment (root cause: the 1px divider was painted 1px
//! inside the sidebar frame edge, leaving a 1px strip of sidebar fill
//! between the content background and the border -- a visible light stripe
//! on the left side of the border. The divider must sit exactly on the
//! sidebar edge with no fill stripe, while staying fully inside the clip
//! rect on every content width, including subpixel jitter from translated
//! labels).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

/// Renders the desktop shell nesting (right panel + scroll + sidebar) with
/// the given labels and returns `(divider_left, divider_right, clip_min_x)`
/// of the divider, whether painted as a line or a 1px fill.
fn divider_geometry(labels: &[String]) -> Option<(f32, f32, f32)> {
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
            Some((points[0].x, points[0].x, clipped.clip_rect.min.x))
        }
        Shape::Rect(shape)
            if (shape.rect.max.x - shape.rect.min.x) < 2.0
                && (shape.rect.max.y - shape.rect.min.y) > 100.0 =>
        {
            Some((shape.rect.min.x, shape.rect.max.x, clipped.clip_rect.min.x))
        }
        _ => None,
    })
}

#[test]
fn divider_sits_on_sidebar_edge_without_fill_stripe() {
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
        let (left, right, clip_min_x) = divider_geometry(labels).expect("divider must be painted");
        assert!(
            left - clip_min_x >= -0.1,
            "divider must stay inside the clip rect, got left {left:.2} before clip {clip_min_x:.2} for {labels:?}"
        );
        assert!(
            left - clip_min_x <= 0.6,
            "no fill stripe may sit left of the divider, got gap {:.2} for {labels:?}",
            left - clip_min_x
        );
        assert!(
            (0.9..=1.1).contains(&(right - left)),
            "divider must be exactly 1px wide, got {:.2} for {labels:?}",
            right - left
        );
    }
}
