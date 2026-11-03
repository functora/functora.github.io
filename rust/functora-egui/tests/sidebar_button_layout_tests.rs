//! Sidebar button layout: item buttons match the expand/collapse/close toggle
//! height, never overlap it, and stay left-aligned with a calculated right gap
//! in every layout (desktop inline panel, desktop rail, mobile overlay).

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

use functora_egui::{Button, ButtonVariant, ComponentSize, LucideIcon, Sidebar, Spacing};

const DESKTOP: Vec2 = Vec2::new(1280.0, 800.0);
const MOBILE: Vec2 = Vec2::new(375.0, 667.0);

fn run_frames(
    ctx: &Context,
    screen: Vec2,
    frames: u32,
    mut frame: impl FnMut(&mut egui::Ui),
) -> egui::FullOutput {
    (0..frames)
        .map(|idx| {
            let raw = RawInput {
                screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
                time: Some(f64::from(idx) / 60.0),
                ..Default::default()
            };
            let mut out = ctx.run_ui(raw, |ui| frame(ui));
            out.textures_delta.clear();
            out
        })
        .last()
        .unwrap_or_default()
}

fn rect_shapes(shapes: &[egui::epaint::ClippedShape]) -> Vec<Rect> {
    shapes
        .iter()
        .filter_map(|cs| match &cs.shape {
            Shape::Rect(rs) => Some(rs.rect),
            _ => None,
        })
        .collect()
}

/// Sidebar item buttons: wide rects painted with the given item height.
fn sidebar_items(rects: &[Rect], height: f32) -> Vec<Rect> {
    rects
        .iter()
        .copied()
        .filter(|r| (r.height() - height).abs() < 1.0 && r.width() > 2.0 * height)
        .collect()
}

/// Expand/collapse or close toggle: square toggle-size rect furthest to the right.
fn sidebar_toggle(rects: &[Rect], height: f32) -> Option<Rect> {
    rects
        .iter()
        .copied()
        .filter(|r| (r.width() - height).abs() < 1.0 && (r.height() - height).abs() < 1.0)
        .max_by(|a, b| a.min.x.total_cmp(&b.min.x))
}

#[test]
fn sidebar_items_match_toggle_height_without_overlap_on_desktop() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let item_height = ComponentSize::Sm.metrics_for(&Spacing::desktop()).0;
    let mut collapsed = false;
    let out = run_frames(&ctx, DESKTOP, 1, |ui| {
        _ = egui::Panel::right("sidebar_panel")
            .default_size(300.0)
            .show(ui, |sidebar_ui| {
                let _ = Sidebar::new().width(256.0).collapsible().show(
                    sidebar_ui,
                    &mut collapsed,
                    |content_ui| {
                        let _ = content_ui.add(
                            Button::new("Home")
                                .icon(LucideIcon::House)
                                .variant(ButtonVariant::Ghost)
                                .size(ComponentSize::Sm)
                                .full_width(),
                        );
                        let _ = content_ui.add(
                            Button::new("About")
                                .icon(LucideIcon::Info)
                                .variant(ButtonVariant::Ghost)
                                .size(ComponentSize::Sm)
                                .full_width(),
                        );
                    },
                );
            });
    });
    let rects = rect_shapes(&out.shapes);
    let items = sidebar_items(&rects, item_height);
    let toggle = sidebar_toggle(&rects, item_height)
        .unwrap_or_else(|| panic!("expand/collapse toggle must be painted as a square rect"));
    assert!(
        items.len() >= 2,
        "sidebar item buttons must be painted, got {items:?}",
    );
    assert!(
        (toggle.height() - item_height).abs() < 0.5,
        "toggle height must equal ComponentSize::Sm, got {toggle:?}",
    );
    for item in &items {
        assert!(
            (item.height() - item_height).abs() < 0.5,
            "sidebar item height must match the toggle height, got {item:?}",
        );
    }
    // Same height line: the toggle starts at the same top as the first item.
    assert!(
        (toggle.min.y - items[0].min.y).abs() < 1.0,
        "toggle must be on the same height as the first sidebar button, got toggle {toggle:?} and item {first_item:?}",
        first_item = items[0]
    );
    // Items are aligned by the left border of the sidebar.
    for item in &items {
        assert!(
            (item.min.x - items[0].min.x).abs() < 1.0,
            "sidebar items must be aligned by the left border of the sidebar"
        );
    }
}

#[test]
fn sidebar_items_match_close_height_without_overlap_on_mobile_overlay() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let item_height = ComponentSize::Sm.metrics_for(&Spacing::mobile()).0;
    let mut collapsed = false;
    let out = run_frames(&ctx, MOBILE, 20, |ui| {
        _ = Sidebar::new()
            .width(256.0)
            .collapsible()
            .show(ui, &mut collapsed, |content_ui| {
                let _ = content_ui.add(
                    Button::new("Home")
                        .icon(LucideIcon::House)
                        .variant(ButtonVariant::Ghost)
                        .size(ComponentSize::Sm)
                        .full_width(),
                );
                let _ = content_ui.add(
                    Button::new("About")
                        .icon(LucideIcon::Info)
                        .variant(ButtonVariant::Ghost)
                        .size(ComponentSize::Sm)
                        .full_width(),
                );
            });
    });
    let rects = rect_shapes(&out.shapes);
    let items = sidebar_items(&rects, item_height);
    let toggle = sidebar_toggle(&rects, item_height)
        .unwrap_or_else(|| panic!("mobile close toggle must be painted as a square rect"));
    assert!(
        items.len() >= 2,
        "sidebar item buttons must be painted, got {items:?}",
    );
    assert!(
        (toggle.height() - item_height).abs() < 0.5,
        "toggle height must equal ComponentSize::Sm on mobile, got {toggle:?}",
    );
    for item in &items {
        assert!(
            (item.height() - item_height).abs() < 0.5,
            "sidebar item height must match the toggle height on mobile, got {item:?}",
        );
    }
    assert!(
        (toggle.min.y - items[0].min.y).abs() < 1.0,
        "toggle must be on the same height as the first sidebar button, got toggle {toggle:?} and item {first_item:?}",
        first_item = items[0]
    );
    for item in &items {
        assert!(
            (item.min.x - items[0].min.x).abs() < 1.0,
            "sidebar items must be aligned by the left border of the sidebar"
        );
    }
}

#[test]
fn sidebar_rail_shows_only_toggle_without_items() {
    let ctx = Context::default();
    functora_egui::setup_fonts(&ctx);
    let item_height = ComponentSize::Sm.metrics_for(&Spacing::desktop()).0;
    let mut collapsed = true;
    let out = run_frames(&ctx, DESKTOP, 1, |ui| {
        _ = egui::Panel::right("sidebar_panel")
            .default_size(64.0)
            .show(ui, |sidebar_ui| {
                let _ = Sidebar::new().width(40.0).collapsible().show(
                    sidebar_ui,
                    &mut collapsed,
                    |content_ui| {
                        let _ = content_ui.add(
                            Button::new("Home")
                                .icon(LucideIcon::House)
                                .variant(ButtonVariant::Ghost)
                                .size(ComponentSize::Sm)
                                .full_width(),
                        );
                    },
                );
            });
    });
    let rects = rect_shapes(&out.shapes);
    let items = sidebar_items(&rects, item_height);
    assert!(
        items.is_empty(),
        "collapsed rail sidebar must not render items, got {items:?}",
    );
    let toggle = sidebar_toggle(&rects, item_height)
        .unwrap_or_else(|| panic!("rail toggle must be painted as a square rect"));
    assert!(
        (toggle.height() - item_height).abs() < 0.5,
        "rail toggle height must equal ComponentSize::Sm, got {toggle:?}",
    );
}
