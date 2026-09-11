#![cfg(feature = "markdown")]
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Markdown viewer must be theme-aware: rendered elements must paint shadcn
//! tokens (code bg, text, links, headings, checkboxes), not stock egui
//! light-leaning visuals.

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
            Shape::Text(text) => Some(
                text.galley
                    .job
                    .sections
                    .iter()
                    .map(|section| section.format.color),
            ),
            _ => None,
        })
        .flatten()
        .collect()
}

fn text_backgrounds(shapes: &[Shape]) -> Vec<Color32> {
    shapes
        .iter()
        .filter_map(|shape| match shape {
            Shape::Text(text) => Some(
                text.galley
                    .job
                    .sections
                    .iter()
                    .map(|section| section.format.background),
            ),
            _ => None,
        })
        .flatten()
        .collect()
}

#[test]
fn dark_theme_code_block_uses_shadcn_secondary_bg() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let shapes = rendered_shapes(Theme::Dark, "```\nlet x = 1;\n```");
    let fills = fills(&shapes);
    assert!(
        fills.contains(&dark.secondary),
        "code block must paint shadcn secondary bg in dark theme, got fills {fills:?}",
    );
}

#[test]
fn light_theme_code_block_uses_shadcn_secondary_bg() {
    let light = functora_egui::theme::shadcn_theme_light::light();
    let shapes = rendered_shapes(Theme::Light, "```\nlet x = 1;\n```");
    let fills = fills(&shapes);
    assert!(
        fills.contains(&light.secondary),
        "code block must paint shadcn secondary bg in light theme, got fills {fills:?}",
    );
}

#[test]
fn dark_theme_inline_code_uses_shadcn_accent_bg() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let shapes = rendered_shapes(Theme::Dark, "inline `code` here");
    let backgrounds = text_backgrounds(&shapes);
    assert!(
        backgrounds.contains(&dark.accent),
        "inline code must paint shadcn accent bg in dark theme, got backgrounds {backgrounds:?}",
    );
}

#[test]
fn light_theme_inline_code_uses_shadcn_accent_bg() {
    let light = functora_egui::theme::shadcn_theme_light::light();
    let shapes = rendered_shapes(Theme::Light, "inline `code` here");
    let backgrounds = text_backgrounds(&shapes);
    assert!(
        backgrounds.contains(&light.accent),
        "inline code must paint shadcn accent bg in light theme, got backgrounds {backgrounds:?}",
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
fn dark_theme_list_marker_uses_shadcn_foreground() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let shapes = rendered_shapes(Theme::Dark, "- item one\n- item two");
    let hits: Vec<Color32> = shapes
        .iter()
        .filter_map(|shape| match shape {
            Shape::Circle(c) => Some(c.fill),
            _ => None,
        })
        .collect();
    assert!(
        hits.contains(&dark.foreground),
        "list markers must paint shadcn foreground in dark theme, got circle fills {hits:?}",
    );
}

fn text_mesh_colors(shapes: &[Shape]) -> Vec<Color32> {
    shapes
        .iter()
        .flat_map(|shape| match shape {
            Shape::Text(text) => text
                .galley
                .rows
                .iter()
                .flat_map(|row| {
                    row.row
                        .visuals
                        .mesh
                        .vertices
                        .iter()
                        .map(|vertex| vertex.color)
                })
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}

fn text_fallback_colors(shapes: &[Shape]) -> Vec<Color32> {
    shapes
        .iter()
        .filter_map(|shape| match shape {
            Shape::Text(text) => Some(text.fallback_color),
            _ => None,
        })
        .collect()
}

#[test]
fn dark_theme_link_uses_shadcn_primary() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let shapes = rendered_shapes(Theme::Dark, "See [example](https://example.com) now");
    let colors = text_fallback_colors(&shapes);
    assert!(
        colors.contains(&dark.primary),
        "link must paint shadcn primary in dark theme, got fallback colors {colors:?}",
    );
}

#[test]
fn light_theme_link_uses_shadcn_primary() {
    let light = functora_egui::theme::shadcn_theme_light::light();
    let shapes = rendered_shapes(Theme::Light, "See [example](https://example.com) now");
    let colors = text_fallback_colors(&shapes);
    assert!(
        colors.contains(&light.primary),
        "link must paint shadcn primary in light theme, got fallback colors {colors:?}",
    );
}

#[test]
fn dark_theme_body_text_uses_shadcn_foreground() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let shapes = rendered_shapes(Theme::Dark, "plain body text");
    let colors = text_fallback_colors(&shapes);
    assert!(
        colors.contains(&dark.foreground),
        "body text must paint shadcn foreground in dark theme, got fallback colors {colors:?}",
    );
}

#[test]
fn dark_theme_selection_uses_shadcn_primary() {
    let dark = functora_egui::theme::shadcn_theme_dark::dark();
    let ctx = Context::default();
    set_theme(&ctx, Theme::Dark);
    let mut cache = functora_egui::CommonMarkCache::default();
    let raw = || RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
        ..Default::default()
    };
    let show = |ui: &mut egui::Ui, view_cache: &mut functora_egui::CommonMarkCache| {
        _ = functora_egui::markdown_view::show(ui, view_cache, "```\nlet x = 1;\nlet y = 2;\n```");
    };

    let mut layout = ctx.run_ui(raw(), |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| show(inner, &mut cache));
    });
    layout.textures_delta.clear();
    let code_rect = layout
        .shapes
        .iter()
        .filter_map(|c| match &c.shape {
            Shape::Rect(r) if r.fill == dark.secondary => Some(r.rect),
            _ => None,
        })
        .max_by(|a, b| a.area().total_cmp(&b.area()))
        .expect("code block must be painted");
    let start = Pos2::new(code_rect.left() + 4.0, code_rect.center().y);
    let end = Pos2::new(code_rect.right() - 4.0, code_rect.center().y + 6.0);

    let mut pressed = raw();
    pressed.events.push(egui::Event::PointerButton {
        pos: start,
        button: egui::PointerButton::Primary,
        pressed: true,
        modifiers: egui::Modifiers::default(),
    });
    let mut started = ctx.run_ui(pressed, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| show(inner, &mut cache));
    });
    started.textures_delta.clear();

    let mut dragged = raw();
    dragged.events.push(egui::Event::PointerMoved(end));
    let mut selected = ctx.run_ui(dragged, |ui| {
        let _ = egui::CentralPanel::default().show(ui, |inner| show(inner, &mut cache));
    });
    selected.textures_delta.clear();

    let colors = text_mesh_colors(
        &selected
            .shapes
            .into_iter()
            .map(|c| c.shape)
            .collect::<Vec<_>>(),
    );
    assert!(
        colors.contains(&dark.primary),
        "text selection must paint shadcn primary in dark theme, got mesh colors {colors:?}",
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
                inner.style().visuals.code_bg_color,
                inner.style().visuals.selection.bg_fill,
                inner.style().visuals.selection.stroke.color,
            );
            _ = functora_egui::markdown_view::show(inner, &mut cache, "plain text");
            after.set(Some((
                before,
                (
                    inner.style().visuals.override_text_color,
                    inner.style().visuals.widgets.active.fg_stroke.color,
                    inner.style().visuals.code_bg_color,
                    inner.style().visuals.selection.bg_fill,
                    inner.style().visuals.selection.stroke.color,
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
