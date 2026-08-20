use egui::{CentralPanel, Context, Panel, Pos2, RawInput, Rect, Vec2};
use egui_shadcn::{
    theme::shadcn_theme_dark, Button, ButtonVariant, Card, ComponentSize, Input, LucideIcon,
    Progress, SelectValue, ShadcnThemeExt, Textarea, ToggleGroup,
};

const CONTENT_MAX_WIDTH: f32 = 960.0;

#[test]
fn content_column_spans_full_height() {
    let ctx = Context::default();
    ctx.set_shadcn_theme(shadcn_theme_dark::dark());
    let mut nav_rect = Rect::NOTHING;
    let mut central_rect = Rect::NOTHING;
    let mut scroll_inner = Rect::NOTHING;
    for _ in 0..3 {
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1200.0, 800.0))),
            ..Default::default()
        };
        let mut out = ctx.run_ui(raw, |ui| {
            let _nav = Panel::top("nav").show(ui, |nav| {
                let _ = nav.horizontal(|row| {
                    _ = row.label("🔐 Cryptonote");
                    row.add_space(16.0);
                    let tabs = vec!["Home".to_string(), "Open".to_string()];
                    let mut index = 0;
                    _ = ToggleGroup::new(tabs).show(row, &mut index);
                    let _ = row.with_layout(
                        egui::Layout::right_to_left(egui::Align::Center),
                        |right| {
                            _ = right.add(
                                Button::icon_only(LucideIcon::Moon)
                                    .variant(ButtonVariant::Outline)
                                    .size(ComponentSize::Sm),
                            );
                            let mut lang = String::from("eng");
                            _ = right.add(SelectValue::new(&mut lang, &["English".to_string()]));
                        },
                    );
                });
                nav_rect = nav.max_rect();
            });
            let central = CentralPanel::default().show(ui, |central| {
                let available = central.available_width();
                let width = available.min(CONTENT_MAX_WIDTH);
                let margin = (available - width) * 0.5;
                let _ = central.with_layout(egui::Layout::left_to_right(egui::Align::Min), |row| {
                    row.add_space(margin);
                    let _ = row.vertical(|col| {
                        col.set_max_width(width);
                        let inner = egui::ScrollArea::vertical()
                            .auto_shrink([false, false])
                            .show(col, |scroll| {
                                let _ = Card::new().heading("Create a note").show(scroll, |card| {
                                    _ = card.add(
                                        Textarea::new(&mut String::new())
                                            .placeholder("Note")
                                            .min_height(240.0),
                                    );
                                });
                                let mut cipher = String::from("none");
                                _ = scroll.add(SelectValue::new(
                                    &mut cipher,
                                    &["No encryption".to_string(), "AES-256-GCM".to_string()],
                                ));
                                let mut pwd = String::new();
                                _ = scroll
                                    .add(Input::new(&mut pwd).password().placeholder("Password"));
                                _ = scroll.add(Progress::new(0.5));
                            });
                        scroll_inner = inner.inner_rect;
                    });
                });
            });
            central_rect = central.response.rect;
        });
        out.textures_delta.clear();
    }
    assert!(
        nav_rect.height() < 80.0,
        "nav panel must stay a thin strip, got {nav_rect:?}"
    );
    assert!(
        central_rect.height() > 600.0,
        "central panel must span the window height, got {central_rect:?}"
    );
    assert!(
        scroll_inner.height() > 600.0,
        "scroll viewport must span the window height, got {scroll_inner:?}"
    );
}
