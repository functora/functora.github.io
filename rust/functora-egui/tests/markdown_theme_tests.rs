#![cfg(feature = "markdown")]
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Markdown viewer must be theme-aware: in dark theme it must paint shadcn
//! dark tokens (code bg, text, links, headings), not stock egui light-leaning
//! visuals.

use egui::{Color32, Context, Pos2, RawInput, Rect, Shape, Vec2};
use functora_egui::theme_extra::{Theme, set_theme};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

fn rendered_shapes(theme: Theme, source: &str) -> Vec<Shape> {
    let ctx = Context::default();
    set_theme(&ctx, theme);
    let mut cache = functora_egui::CommonMarkCache::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            _ = functora_egui::markdown_view::show(inner, &mut cache, source);
        });
    });
    out.textures_delta.clear();
    out.shapes.into_iter().map(|c| c.shape).collect()
}

fn fills(shapes: &[Shape]) -> Vec<Color32> {
    shapes
        .iter()
        .filter_map(|shape| match shape {
            Shape::Rect(rect) => Some(rect.fill),
            _ => None,
        })
        .collect()
}

fn text_colors(shapes: &[Shape]) -> Vec<Color32> {
    shapes
        .iter()
        .filter_map(|shape| match shape {
            Shape::Text(text) => {
                let mut colors = Vec::new();
                for section in &text.galley.job.sections {
                    colors.push(section.format.color);
                }
                Some(colors)
            }
            _ => None,
        })
        .flatten()
        .collect()
}

#[test]
fn dark_theme_code_block_uses_shadcn_muted_bg() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let shapes = rendered_shapes(Theme::Dark, "```\nlet x = 1;\n```");
    let fills = fills(&shapes);
    assert!(
        fills.contains(&dark.muted),
        "code block must paint shadcn muted bg in dark theme, got fills {fills:?}",
    );
}

#[test]
fn light_theme_code_block_uses_shadcn_muted_bg() {
    let light = functora_egui::theme::shadcn_theme_light::light();
    let shapes = rendered_shapes(Theme::Light, "```\nlet x = 1;\n```");
    let fills = fills(&shapes);
    assert!(
        fills.contains(&light.muted),
        "code block must paint shadcn muted bg in light theme, got fills {fills:?}",
    );
}

#[test]
fn dark_theme_heading_uses_shadcn_foreground() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let shapes = rendered_shapes(Theme::Dark, "# Showcase");
    let colors = text_colors(&shapes);
    assert!(
        colors.contains(&dark.foreground),
        "heading must paint shadcn foreground in dark theme, got text colors {colors:?}",
    );
}

#[test]
fn light_theme_heading_uses_shadcn_foreground() {
    let light = functora_egui::theme::shadcn_theme_light::light();
    let shapes = rendered_shapes(Theme::Light, "# Showcase");
    let colors = text_colors(&shapes);
    assert!(
        colors.contains(&light.foreground),
        "heading must paint shadcn foreground in light theme, got text colors {colors:?}",
    );
}

#[test]
fn theme_restored_after_render() {
    let ctx = Context::default();
    set_theme(&ctx, Theme::Dark);
    let mut cache = functora_egui::CommonMarkCache::default();
    let after = std::cell::Cell::new(None);
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        ..Default::default()
    };
    let mut out = ctx.run_ui(raw, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| {
            let before = (
                inner.style().visuals.override_text_color,
                inner.style().visuals.widgets.active.fg_stroke.color,
            );
            _ = functora_egui::markdown_view::show(inner, &mut cache, "plain text");
            after.set(Some((
                before,
                (
                    inner.style().visuals.override_text_color,
                    inner.style().visuals.widgets.active.fg_stroke.color,
                ),
            )));
        });
    });
    out.textures_delta.clear();
    let (before, restored) = after.get().expect("body must run");
    assert_eq!(
        before, restored,
        "markdown render must restore original visuals",
    );
}
