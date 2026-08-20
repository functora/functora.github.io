use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
use egui_shadcn::{
    Breakpoint, Button, ComponentSize, Dialog, Flex, Input, ResponsiveExt, SelectValue, Sheet,
    SheetSide, Sidebar, Spacing,
};

const MOBILE: Vec2 = Vec2::new(375.0, 667.0);
const DESKTOP: Vec2 = Vec2::new(1280.0, 800.0);

fn collect_filled_rects(shapes: &[egui::epaint::ClippedShape]) -> Vec<Rect> {
    shapes
        .iter()
        .filter_map(|cs| match &cs.shape {
            Shape::Rect(rs) => Some(vec![rs.rect]),
            Shape::Vec(v) => Some(
                v.iter()
                    .filter_map(|s| match s {
                        Shape::Rect(rs) => Some(rs.rect),
                        _ => None,
                    })
                    .collect(),
            ),
            _ => None,
        })
        .flatten()
        .collect()
}

fn run_frames(
    ctx: &Context,
    screen: Vec2,
    frames: usize,
    mut frame: impl FnMut(&mut egui::Ui),
) -> egui::FullOutput {
    let mut out = egui::FullOutput::default();
    for i in 0..frames {
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, screen)),
            time: Some(i as f64 / 60.0),
            ..Default::default()
        };
        out = ctx.run_ui(raw, |ui| frame(ui));
        out.textures_delta.clear();
    }
    out
}

fn find_rect(rects: &[Rect], predicate: impl Fn(Rect) -> bool) -> Option<Rect> {
    rects.iter().copied().find(|r| predicate(*r))
}

#[test]
fn breakpoint_classifies_viewport_widths() {
    assert_eq!(Breakpoint::from_width(0.0), Breakpoint::Mobile);
    assert_eq!(Breakpoint::from_width(375.0), Breakpoint::Mobile);
    assert_eq!(
        Breakpoint::from_width(Breakpoint::MOBILE_MAX_WIDTH - 0.1),
        Breakpoint::Mobile
    );
    assert_eq!(
        Breakpoint::from_width(Breakpoint::MOBILE_MAX_WIDTH),
        Breakpoint::Desktop
    );
    assert_eq!(Breakpoint::from_width(1280.0), Breakpoint::Desktop);
}

#[test]
fn breakpoint_follows_viewport_width() {
    let ctx = Context::default();
    run_frames(&ctx, MOBILE, 1, |ui| {
        assert!(ui.ctx().breakpoint().is_mobile());
        assert!(ui.on_mobile());
        assert!(!ui.on_desktop());
    });
    run_frames(&ctx, DESKTOP, 1, |ui| {
        assert!(ui.ctx().breakpoint().is_desktop());
        assert!(ui.on_desktop());
        assert!(!ui.on_mobile());
    });
}

#[test]
fn spacing_scale_matches_functora_css_conventions() {
    assert_eq!(Spacing::desktop().content_max_width, 1440.0); // @width: 90rem
    assert_eq!(Spacing::mobile().content_max_width, 1440.0);
    assert_eq!(Spacing::mobile().page_padding, 16.0);
    assert_eq!(Spacing::desktop().page_padding, 32.0); // 2rem section padding
    assert_eq!(Spacing::mobile().touch_height, 44.0);
    assert_eq!(Spacing::desktop().touch_height, 32.0);
}

#[test]
fn component_sizes_scale_for_touch_on_mobile() {
    assert_eq!(
        ComponentSize::Default.metrics_for(&Spacing::desktop()),
        (32.0, 10.0, 14.0)
    );
    assert_eq!(
        ComponentSize::Default.metrics_for(&Spacing::mobile()),
        (44.0, 14.0, 14.0)
    );
    assert_eq!(
        ComponentSize::Xs.metrics_for(&Spacing::mobile()),
        (36.0, 12.0, 12.0)
    );
    assert_eq!(
        ComponentSize::Lg.metrics_for(&Spacing::mobile()),
        (48.0, 14.0, 14.0)
    );
}

#[test]
fn buttons_reach_touch_height_on_mobile() {
    let ctx = Context::default();
    let mut mobile_height = 0.0;
    run_frames(&ctx, MOBILE, 1, |ui| {
        let resp = ui.add(Button::new("Click me"));
        mobile_height = resp.rect.height();
        let small = ui.add(Button::new("Small").size(ComponentSize::Sm));
        assert_eq!(small.rect.height(), 40.0);
    });
    assert_eq!(mobile_height, 44.0);

    let mut desktop_height = 0.0;
    run_frames(&ctx, DESKTOP, 1, |ui| {
        let resp = ui.add(Button::new("Click me"));
        desktop_height = resp.rect.height();
    });
    assert_eq!(desktop_height, 32.0);
}

#[test]
fn form_controls_use_touch_height_on_mobile() {
    let ctx = Context::default();
    let mut select_height = 0.0;
    let out = run_frames(&ctx, MOBILE, 1, |ui| {
        let mut text = String::new();
        ui.add(Input::new(&mut text).placeholder("Type"));
        let mut selected = String::new();
        let select = ui.add(SelectValue::new(&mut selected, &["a".to_owned()]));
        select_height = select.rect.height();
    });
    assert_eq!(select_height, 44.0);
    let rects = collect_filled_rects(&out.shapes);
    let input = find_rect(&rects, |r| {
        (r.height() - 44.0).abs() < 0.5 && r.width() > 300.0
    })
    .expect("input field must be drawn with touch height");
    assert!(
        (input.width() - MOBILE.x).abs() < 1.0,
        "input must fill the mobile width, got {input:?}"
    );
}

#[test]
fn input_height_follows_spacing_on_desktop() {
    let ctx = Context::default();
    let out = run_frames(&ctx, DESKTOP, 1, |ui| {
        let mut text = String::new();
        ui.add(Input::new(&mut text).placeholder("Type"));
    });
    let rects = collect_filled_rects(&out.shapes);
    let input = find_rect(&rects, |r| {
        (r.height() - 32.0).abs() < 0.5 && r.width() > 300.0
    })
    .expect("input field must be drawn");
    assert!(
        (input.width() - DESKTOP.x).abs() < 1.0,
        "input must fill the desktop width, got {input:?}"
    );
}

#[test]
fn dialog_becomes_bottom_sheet_on_mobile() {
    let ctx = Context::default();
    let mut open = true;
    let out = run_frames(&ctx, MOBILE, 3, |ui| {
        Dialog::new().title("Test").show(ui.ctx(), &mut open, |d| {
            d.label("body");
        });
    });
    let rects = collect_filled_rects(&out.shapes);
    let panel = find_rect(&rects, |r| {
        r.max.y > MOBILE.y - 1.0
            && r.min.y < MOBILE.y - 20.0
            && r.width() > 300.0
            && r.width() < MOBILE.x - 0.5
    })
    .expect("bottom-anchored dialog panel must be drawn");
    assert!(
        (panel.width() - (MOBILE.x - 2.0 * Spacing::mobile().page_padding)).abs() < 2.0,
        "panel must span the screen minus page padding, got {panel:?}"
    );
    assert!(
        panel.min.x >= -0.5 && panel.max.x <= MOBILE.x + 0.5,
        "panel must stay inside the screen, got {panel:?}"
    );
}

#[test]
fn dialog_stays_centered_on_desktop() {
    let ctx = Context::default();
    let mut open = true;
    let out = run_frames(&ctx, DESKTOP, 3, |ui| {
        Dialog::new().title("Test").show(ui.ctx(), &mut open, |d| {
            d.label("body");
        });
    });
    let rects = collect_filled_rects(&out.shapes);
    let panel = find_rect(&rects, |r| {
        r.width() < DESKTOP.x - 0.5 && (r.center().x - DESKTOP.x / 2.0).abs() < 2.0
    })
    .expect("centered dialog panel must be drawn");
    assert!(
        (panel.center().x - DESKTOP.x / 2.0).abs() < 2.0,
        "panel must be horizontally centered, got {panel:?}"
    );
}

#[test]
fn sheet_clamps_width_to_viewport_on_mobile() {
    let ctx = Context::default();
    let mut open = true;
    let out = run_frames(&ctx, MOBILE, 20, |ui| {
        Sheet::new()
            .side(SheetSide::Right)
            .width(400.0)
            .title("Sheet")
            .show(ui.ctx(), &mut open, |sheet| {
                sheet.label("body");
            });
    });
    let rects = collect_filled_rects(&out.shapes);
    let panel = find_rect(&rects, |r| {
        r.max.x > MOBILE.x - 1.0 && r.width() > 300.0 && r.width() < MOBILE.x - 0.5
    })
    .expect("right sheet panel must be drawn");
    assert!(
        (panel.width() - (MOBILE.x - 48.0)).abs() < 2.0,
        "panel must be clamped to the screen minus margins, got {panel:?}"
    );
    assert!(
        panel.min.x >= -0.5 && panel.max.y <= MOBILE.y + 0.5,
        "panel must stay inside the screen, got {panel:?}"
    );
}

#[test]
fn responsive_sidebar_hides_when_collapsed_on_mobile() {
    let ctx = Context::default();
    let mut collapsed = true;
    let mut content_called = false;
    let mut sidebar_rect = Rect::NOTHING;
    run_frames(&ctx, MOBILE, 3, |ui| {
        let resp = Sidebar::new().collapsible().show(ui, &mut collapsed, |_| {
            content_called = true;
        });
        sidebar_rect = resp.rect;
    });
    assert!(!content_called, "content must be hidden while collapsed");
    assert!(
        sidebar_rect.width() == 0.0,
        "collapsed sidebar must take no space"
    );
}

#[test]
fn responsive_sidebar_draws_overlay_when_open_on_mobile() {
    let ctx = Context::default();
    let mut collapsed = false;
    let mut content_called = false;
    let out = run_frames(&ctx, MOBILE, 20, |ui| {
        Sidebar::new().collapsible().show(ui, &mut collapsed, |_| {
            content_called = true;
        });
    });
    assert!(content_called);
    let rects = collect_filled_rects(&out.shapes);
    let panel = find_rect(&rects, |r| {
        r.min.x > -1.0
            && r.min.x < 1.0
            && r.width() > 250.0
            && r.width() < MOBILE.x - 0.5
            && r.height() > 600.0
    })
    .expect("left overlay panel must be drawn");
    assert!(
        panel.max.x <= MOBILE.x && panel.max.y <= MOBILE.y,
        "overlay panel must stay inside the screen, got {panel:?}"
    );
}

#[test]
fn responsive_sidebar_collapses_to_rail_on_desktop() {
    let ctx = Context::default();
    let mut collapsed = true;
    run_frames(&ctx, DESKTOP, 1, |ui| {
        let resp = Sidebar::new()
            .collapsible()
            .show(ui, &mut collapsed, |_| {});
        assert!(
            resp.rect.width() < 100.0,
            "collapsed sidebar must be a narrow icon rail, got {:?}",
            resp.rect
        );
    });

    let mut expanded = false;
    run_frames(&ctx, DESKTOP, 1, |ui| {
        let resp = Sidebar::new().collapsible().show(ui, &mut expanded, |_| {});
        assert!(
            resp.rect.width() > 250.0,
            "expanded sidebar must be full width, got {:?}",
            resp.rect
        );
    });
}

#[test]
fn static_sidebar_stays_inline_on_mobile() {
    let ctx = Context::default();
    let mut collapsed = false;
    let mut content_called = false;
    run_frames(&ctx, MOBILE, 1, |ui| {
        let resp = Sidebar::new().static_().show(ui, &mut collapsed, |_| {
            content_called = true;
        });
        assert!(
            resp.rect.width() > 250.0,
            "static sidebar must render its inline panel on mobile, got {:?}",
            resp.rect
        );
    });
    assert!(content_called, "inline panel must always show its content");

    let mut after_x = 0.0;
    run_frames(&ctx, MOBILE, 1, |ui| {
        ui.horizontal(|ui| {
            Sidebar::new().static_().show(ui, &mut collapsed, |_| {});
            let (rect, _) = ui.allocate_exact_size(egui::vec2(10.0, 10.0), egui::Sense::hover());
            after_x = rect.min.x;
        });
    });
    assert!(
        after_x > 250.0,
        "static sidebar must occupy layout space, got {after_x}"
    );
}

#[test]
fn sidebar_toggle_button_uses_touch_height() {
    let ctx = Context::default();
    let mut collapsed = true;
    run_frames(&ctx, MOBILE, 1, |ui| {
        let resp = Sidebar::toggle_button(ui, &mut collapsed);
        assert_eq!(resp.rect.width(), 44.0);
        assert_eq!(resp.rect.height(), 44.0);
    });
    run_frames(&ctx, DESKTOP, 1, |ui| {
        let resp = Sidebar::toggle_button(ui, &mut collapsed);
        assert_eq!(resp.rect.width(), 32.0);
    });
}

#[test]
fn flex_wraps_on_mobile_but_not_desktop() {
    let ctx = Context::default();
    let mut mobile_height = 0.0;
    run_frames(&ctx, MOBILE, 1, |ui| {
        let inner = Flex::row().gap(8.0).show(ui, |f| {
            for label in ["One", "Two", "Three", "Four", "Five"] {
                f.add(Button::new(label).shortcut_text("Ctrl+K"));
            }
        });
        mobile_height = inner.response.rect.height();
    });
    assert!(
        mobile_height > 44.0,
        "toolbar must wrap onto multiple lines on mobile, got {mobile_height}"
    );

    let mut desktop_height = 0.0;
    run_frames(&ctx, DESKTOP, 1, |ui| {
        let inner = Flex::row().gap(8.0).show(ui, |f| {
            for label in ["One", "Two", "Three", "Four", "Five"] {
                f.add(Button::new(label).shortcut_text("Ctrl+K"));
            }
        });
        desktop_height = inner.response.rect.height();
    });
    assert!(
        desktop_height <= 44.0,
        "toolbar must stay on one line on desktop, got {desktop_height}"
    );
}

#[test]
fn flex_no_wrap_on_mobile_keeps_single_line() {
    let ctx = Context::default();
    let mut mobile_height = 0.0;
    run_frames(&ctx, MOBILE, 1, |ui| {
        let inner = Flex::row().gap(8.0).no_wrap_on_mobile().show(ui, |f| {
            for label in ["One", "Two", "Three", "Four", "Five"] {
                f.add(Button::new(label).shortcut_text("Ctrl+K"));
            }
        });
        mobile_height = inner.response.rect.height();
    });
    assert!(
        mobile_height <= 44.0,
        "toolbar must stay on one line with no_wrap_on_mobile, got {mobile_height}"
    );
}

#[test]
fn flex_column_does_not_wrap_on_mobile() {
    let ctx = Context::default();
    let mut column_width = 0.0;
    run_frames(&ctx, MOBILE, 1, |ui| {
        let inner = Flex::column().gap(8.0).show(ui, |f| {
            for label in ["One", "Two", "Three", "Four", "Five"] {
                f.add(egui::Button::new(label).min_size(egui::vec2(120.0, 32.0)));
            }
        });
        column_width = inner.response.rect.width();
    });
    assert!(
        column_width < 200.0,
        "column must not wrap into side-by-side columns on mobile, got {column_width}"
    );
}
