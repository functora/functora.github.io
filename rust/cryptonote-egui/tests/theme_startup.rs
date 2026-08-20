use cryptonote_egui::theme::Theme;
use egui::{CentralPanel, Color32, Context, Panel, Pos2, RawInput, Rect, Shape, Vec2};
use elegance::{self, Card, TextArea};

#[test]
fn startup_dark_theme_survives_light_system_theme() {
    let ctx = Context::default();
    let out = run_app_frames(&ctx, |c| Theme::Dark.apply(c), egui::Theme::Light);
    let bg = largest_rect_fill(&out.shapes);
    assert!(
        ctx.global_style().visuals.dark_mode,
        "global style must be dark after slate install even when the system theme is light"
    );
    assert_eq!(elegance::Theme::current(&ctx), elegance::Theme::slate());
    assert_eq!(
        bg,
        Some(Color32::from_rgb(0x0f, 0x17, 0x2a)),
        "background must be slate dark, got {bg:?}"
    );
}

#[test]
fn startup_light_theme_survives_dark_system_theme() {
    let ctx = Context::default();
    let out = run_app_frames(&ctx, |c| Theme::Light.apply(c), egui::Theme::Dark);
    let bg = largest_rect_fill(&out.shapes);
    assert!(
        !ctx.global_style().visuals.dark_mode,
        "global style must be light after frost install even when the system theme is dark"
    );
    assert_eq!(elegance::Theme::current(&ctx), elegance::Theme::frost());
    assert_eq!(
        bg,
        Some(Color32::from_rgb(0xe2, 0xe8, 0xf0)),
        "background must be frost light, got {bg:?}"
    );
}

#[test]
fn toggle_keeps_style_and_widgets_consistent() {
    let ctx = Context::default();
    let apply = |c: &Context, t: Theme| t.apply(c);
    apply(&ctx, Theme::Dark);
    let _ = run_app_frames(&ctx, |_| {}, egui::Theme::Light);
    assert!(ctx.global_style().visuals.dark_mode);
    apply(&ctx, Theme::Light);
    let light_out = run_app_frames(&ctx, |_| {}, egui::Theme::Light);
    assert!(!ctx.global_style().visuals.dark_mode);
    assert_eq!(elegance::Theme::current(&ctx), elegance::Theme::frost());
    assert_eq!(
        largest_rect_fill(&light_out.shapes),
        Some(Color32::from_rgb(0xe2, 0xe8, 0xf0))
    );
    apply(&ctx, Theme::Dark);
    let dark_out = run_app_frames(&ctx, |_| {}, egui::Theme::Light);
    assert!(ctx.global_style().visuals.dark_mode);
    assert_eq!(elegance::Theme::current(&ctx), elegance::Theme::slate());
    assert_eq!(
        largest_rect_fill(&dark_out.shapes),
        Some(Color32::from_rgb(0x0f, 0x17, 0x2a))
    );
}

fn run_app_frames(
    ctx: &Context,
    first_install: impl Fn(&Context),
    system_theme: egui::Theme,
) -> egui::FullOutput {
    first_install(ctx);
    let mut out = egui::FullOutput::default();
    for _ in 0..3 {
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(420.0, 720.0))),
            system_theme: Some(system_theme),
            ..Default::default()
        };
        out = ctx.run_ui(raw, |ui| {
            let _nav = Panel::top("nav").show(ui, |nav| {
                let _ = nav.horizontal(|row| {
                    _ = row.label("🔐 Cryptonote");
                });
            });
            let _central = CentralPanel::default().show(ui, |central| {
                let available = central.available_width();
                let width = available.min(960.0);
                let margin = (available - width) * 0.5;
                let _ = central.with_layout(egui::Layout::left_to_right(egui::Align::Min), |row| {
                    row.add_space(margin);
                    let _ = row.vertical(|col| {
                        col.set_max_width(width);
                        let _inner = egui::ScrollArea::vertical()
                            .auto_shrink([false, false])
                            .show(col, |scroll| {
                                let _ = Card::new().heading("Create a note").show(scroll, |card| {
                                    _ = card.add(
                                        TextArea::new(&mut String::new()).label("Note").rows(12),
                                    );
                                });
                            });
                    });
                });
            });
        });
        out.textures_delta.clear();
    }
    out
}

fn rect_fills(shapes: &[egui::epaint::ClippedShape]) -> Vec<(Rect, Color32)> {
    shapes
        .iter()
        .flat_map(|cs| match &cs.shape {
            Shape::Rect(rs) => vec![(cs.clip_rect, rs.fill)],
            Shape::Vec(v) => v
                .iter()
                .filter_map(|s| match s {
                    Shape::Rect(rs) => Some((cs.clip_rect, rs.fill)),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}

fn largest_rect_fill(shapes: &[egui::epaint::ClippedShape]) -> Option<Color32> {
    rect_fills(shapes)
        .into_iter()
        .max_by(|a, b| (a.0.width() * a.0.height()).total_cmp(&(b.0.width() * b.0.height())))
        .map(|(_, fill)| fill)
}
