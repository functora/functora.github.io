use egui::{CentralPanel, Context, Panel, Pos2, RawInput, Rect, Vec2};
use elegance::{Button, ButtonSize, Card, ProgressBar, Select, TabBar, TextArea, TextInput, Theme};

const CONTENT_MAX_WIDTH: f32 = 960.0;

#[test]
fn content_column_spans_full_height() {
    let ctx = Context::default();
    Theme::slate().install(&ctx);
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
                    _ = row.add(TabBar::new(&mut index, tabs));
                    let _ = row.with_layout(
                        egui::Layout::right_to_left(egui::Align::Center),
                        |right| {
                            _ = right.add(Button::new("🌙").outline().size(ButtonSize::Small));
                            let mut lang = String::from("eng");
                            _ = right.add(
                                Select::new("language", &mut lang)
                                    .options(vec![("eng".to_string(), "English".to_string())]),
                            );
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
                                        TextArea::new(&mut String::new()).label("Note").rows(12),
                                    );
                                });
                                let mut cipher = String::from("none");
                                _ = scroll.add(Select::new("mode", &mut cipher).options(vec![
                                    ("none".to_string(), "No encryption".to_string()),
                                    ("aes".to_string(), "AES-256-GCM".to_string()),
                                ]));
                                let mut pwd = String::new();
                                _ = scroll.add(
                                    TextInput::new(&mut pwd).label("Password").revealable(true),
                                );
                                _ = scroll.add(
                                    ProgressBar::new(0.5)
                                        .text("Encrypt 50%".to_string())
                                        .accent(elegance::Accent::Blue),
                                );
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
