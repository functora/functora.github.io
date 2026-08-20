use egui::{CentralPanel, Context, Panel, Pos2, RawInput, Rect, Shape, Vec2};
use elegance::{Button, ButtonSize, Card, Drawer, DrawerSide, Select, TextArea, TextInput, Theme};

const CONTENT_MAX_WIDTH: f32 = 960.0;

struct Layout {
    nav_rect: Rect,
    central_rect: Rect,
    scroll_inner: Rect,
    scroll_content: Vec2,
}

fn collect_rects(shapes: &[egui::epaint::ClippedShape]) -> Vec<Rect> {
    shapes
        .iter()
        .flat_map(|cs| match &cs.shape {
            Shape::Rect(rs) => vec![rs.rect],
            Shape::Vec(v) => v
                .iter()
                .filter_map(|s| match s {
                    Shape::Rect(rs) => Some(rs.rect),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}

fn run_frames(ctx: &Context, screen: Vec2, nav_open: &mut bool) -> (Layout, Vec<Rect>) {
    let mut layout = Layout {
        nav_rect: Rect::NOTHING,
        central_rect: Rect::NOTHING,
        scroll_inner: Rect::NOTHING,
        scroll_content: Vec2::ZERO,
    };
    let mut all_rects: Vec<Rect> = Vec::new();
    for _ in 0..3 {
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
            ..Default::default()
        };
        let mut out = ctx.run_ui(raw, |ui| {
            let _nav = Panel::top("nav").show(ui, |nav| {
                let _ = nav.horizontal(|row| {
                    let _ = row.add(
                        Button::new(egui::RichText::new("←"))
                            .outline()
                            .size(ButtonSize::Small),
                    );
                    _ = row.label(egui::RichText::new("Cryptonote").strong());
                    let _ = row.with_layout(
                        egui::Layout::right_to_left(egui::Align::Center),
                        |right| {
                            _ = right.add(
                                Button::new(egui::RichText::new("☰"))
                                    .outline()
                                    .size(ButtonSize::Small),
                            );
                        },
                    );
                });
                layout.nav_rect = nav.max_rect();
            });
            let width = (ui.ctx().content_rect().width() * 0.7).clamp(220.0, 300.0);
            let _ = Drawer::new("nav_drawer", nav_open)
                .side(DrawerSide::Right)
                .width(width)
                .title("Cryptonote")
                .show(ui.ctx(), |drawer| {
                    _ = drawer.add(Button::new("Home").outline().full_width());
                });
            let central = CentralPanel::default().show(ui, |central| {
                let available = central.available_width();
                let content_width = available.min(CONTENT_MAX_WIDTH);
                let margin = (available - content_width) * 0.5;
                let _ = central.with_layout(egui::Layout::left_to_right(egui::Align::Min), |row| {
                    row.add_space(margin);
                    let _ = row.vertical(|col| {
                        col.set_max_width(content_width);
                        let inner = egui::ScrollArea::vertical()
                            .auto_shrink([false, false])
                            .show(col, |scroll| {
                                scroll.add_space(8.0);
                                let _ = Card::new().heading("Create a note").show(scroll, |card| {
                                    let _ = card.add(
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
                                    Button::new("Share")
                                        .accent(elegance::Accent::Blue)
                                        .full_width(),
                                );
                            });
                        layout.scroll_inner = inner.inner_rect;
                        layout.scroll_content = inner.content_size;
                    });
                });
            });
            layout.central_rect = central.response.rect;
        });
        all_rects.extend(collect_rects(&out.shapes));
        out.textures_delta.clear();
    }
    (layout, all_rects)
}

#[test]
fn mobile_layout_fits_and_scrolls() {
    let ctx = Context::default();
    Theme::slate().install(&ctx);
    let mut nav_open = false;
    let screen = Vec2::new(375.0, 667.0);
    let (layout, rects) = run_frames(&ctx, screen, &mut nav_open);
    assert!(
        layout.nav_rect.height() < 80.0,
        "nav must be a thin strip, got {:?}",
        layout.nav_rect
    );
    assert!(
        layout.central_rect.height() > 600.0,
        "central must span the window, got {:?}",
        layout.central_rect
    );
    assert!(
        layout.scroll_inner.height() > 600.0,
        "scroll viewport must span the window, got {:?}",
        layout.scroll_inner
    );
    assert!(
        layout.scroll_content.y <= layout.scroll_inner.height() + 0.5,
        "content must fit the viewport vertically at 375x667 (nothing to scroll), got content {:?} vs viewport {:?}",
        layout.scroll_content,
        layout.scroll_inner
    );
    let screen_rect = Rect::from_min_size(Pos2::ZERO, screen);
    for r in rects {
        assert!(
            r.min.x >= screen_rect.min.x - 0.5 && r.max.x <= screen_rect.max.x + 0.5,
            "no content may overflow horizontally, got rect {r:?} in {screen_rect:?}"
        );
    }
}

#[test]
fn mobile_layout_scrolls_when_content_does_not_fit() {
    let ctx = Context::default();
    Theme::slate().install(&ctx);
    let mut nav_open = false;
    let screen = Vec2::new(375.0, 480.0);
    let (layout, _) = run_frames(&ctx, screen, &mut nav_open);
    assert!(
        layout.scroll_content.y > layout.scroll_inner.height(),
        "content must overflow the viewport vertically to be scrollable at 375x480, got content {:?} vs viewport {:?}",
        layout.scroll_content,
        layout.scroll_inner
    );
}

#[test]
fn mobile_drawer_opening_keeps_content_clickable() {
    let ctx = Context::default();
    Theme::slate().install(&ctx);
    let mut nav_open = false;
    let screen = Vec2::new(375.0, 667.0);
    let _ = run_frames(&ctx, screen, &mut nav_open);
    let mut drawer_open = true;
    let (layout, _) = run_frames(&ctx, screen, &mut drawer_open);
    assert!(
        layout.scroll_inner.height() > 600.0,
        "scroll viewport must remain full height with drawer open, got {:?}",
        layout.scroll_inner
    );
}
