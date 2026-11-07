//! Glyph painting consistency: caching the parsed icon geometry must not
//! change the painted output and must stay correct across interleaved icons.

use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::icons::lucide_icon::{ALL, LucideIcon};
use functora_egui::{paint_icon, paint_icon_svg};

const COLOR: egui::Color32 = egui::Color32::from_rgb(240, 240, 240);
const FRESH_PARSES: usize = 9;
const CYCLING_ICONS: usize = 4;
const PAINTS_PER_ICON: usize = 4;

fn screen() -> Rect {
    Rect::from_min_size(Pos2::ZERO, Vec2::new(64.0, 64.0))
}

fn run_frame(ctx: &Context, paint: impl Fn(&egui::Painter)) -> Vec<egui::epaint::ClippedShape> {
    let mut out = ctx.run_ui(
        RawInput {
            screen_rect: Some(screen()),
            time: Some(0.0),
            ..Default::default()
        },
        |ui| paint(&ui.painter_at(screen())),
    );
    out.textures_delta.clear();
    out.shapes
}

fn paint_enum_shapes(
    ctx: &Context,
    rect: Rect,
    icon: LucideIcon,
) -> Vec<egui::epaint::ClippedShape> {
    run_frame(ctx, |painter| paint_icon(painter, rect, &icon, COLOR))
}

fn paint_body_shapes(
    ctx: &Context,
    rect: Rect,
    icon: LucideIcon,
) -> Vec<egui::epaint::ClippedShape> {
    let body = icon.svg_data();
    run_frame(ctx, |painter| paint_icon_svg(painter, rect, body, COLOR))
}

const SPANS: &[f32] = &[12.0, 24.0, 48.0, 64.0];

fn varied_rect(idx: usize) -> Rect {
    let span = SPANS[idx % SPANS.len()];
    Rect::from_min_size(Pos2::ZERO, Vec2::splat(span))
}

fn spread_icons(count: usize) -> Vec<LucideIcon> {
    let stride = ALL.len() / count;
    (0..count).map(|idx| ALL[idx * stride]).collect()
}

#[test]
fn paint_icon_matches_parsing_its_svg_body() {
    for (idx, icon) in spread_icons(FRESH_PARSES).into_iter().enumerate() {
        let ctx = Context::default();
        let rect = varied_rect(idx);
        assert_eq!(
            paint_enum_shapes(&ctx, rect, icon),
            paint_body_shapes(&ctx, rect, icon),
            "glyph geometry from the cache must equal a fresh parse of {icon:?}"
        );
    }
}

#[test]
fn repeated_and_interleaved_paints_are_stable() {
    let ctx = Context::default();
    for icon in spread_icons(CYCLING_ICONS) {
        let paints = (0..PAINTS_PER_ICON)
            .map(|_| paint_enum_shapes(&ctx, screen(), icon))
            .collect::<Vec<_>>();
        for repeat in paints.iter().skip(1) {
            assert_eq!(
                paints.first(),
                Some(repeat),
                "cache must keep {icon:?} stable across paints"
            );
        }
    }
}
