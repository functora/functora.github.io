#![allow(clippy::unwrap_used)]
use egui::{Context, Pos2, RawInput, Rect, Vec2};
use functora_egui::{Button, ButtonGroup, LucideIcon, Separator, Toolbar};

#[test]
fn toolbar_vertical_alignment() {
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(800.0, 600.0))),
        ..Default::default()
    };
    let mut rects: Vec<(String, Rect)> = Vec::new();
    let mut output = ctx.run_ui(raw, |ui| {
        let _ = Toolbar::new().show(ui, |toolbar_ui| {
            let r1 = ButtonGroup::show(toolbar_ui, |group_ui| {
                for icon in [
                    LucideIcon::MousePointer2,
                    LucideIcon::PenTool,
                    LucideIcon::Spline,
                    LucideIcon::Frame,
                    LucideIcon::Type,
                ] {
                    let _ = Button::icon_only(icon).show(group_ui);
                }
            });
            rects.push(("group1".to_string(), r1.response.rect));
            let s1 = Separator::vertical().show(toolbar_ui);
            rects.push(("sep1".to_string(), s1.rect));
            let r2 = ButtonGroup::show(toolbar_ui, |group_ui| {
                let _ = Button::icon_only(LucideIcon::Undo2).show(group_ui);
                let _ = Button::icon_only(LucideIcon::Redo2).show(group_ui);
            });
            rects.push(("group2".to_string(), r2.response.rect));
            let s2 = Separator::vertical().show(toolbar_ui);
            rects.push(("sep2".to_string(), s2.rect));
            let b1 = Button::new("Snap").show(toolbar_ui);
            rects.push(("snap".to_string(), b1.rect));
            let b2 = Button::new("Preview")
                .icon(LucideIcon::Play)
                .show(toolbar_ui);
            rects.push(("preview".to_string(), b2.rect));
        });
    });
    output.textures_delta.clear();
    let g1 = rects[0].1;
    let g2 = rects[2].1;
    assert!(
        (g1.center().y - g2.center().y).abs() < 0.5,
        "groups not aligned: g1 {g1:?} g2 {g2:?}"
    );
    let snap = rects[4].1;
    let preview = rects[5].1;
    assert!(
        (snap.center().y - preview.center().y).abs() < 0.5,
        "buttons not aligned"
    );
    let sep1 = rects[1].1;
    assert!(
        (sep1.center().y - g1.center().y).abs() < 0.5,
        "separator not centered with group"
    );
}

#[test]
fn toolbar_vertical_alignment_wrap_false() {
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(800.0, 600.0))),
        ..Default::default()
    };
    let mut rects: Vec<(String, Rect)> = Vec::new();
    let mut output = ctx.run_ui(raw, |ui| {
        let _ = Toolbar::new().wrap(false).show(ui, |toolbar_ui| {
            let r1 = ButtonGroup::show(toolbar_ui, |group_ui| {
                for icon in [LucideIcon::MousePointer2, LucideIcon::PenTool] {
                    let _ = Button::icon_only(icon).show(group_ui);
                }
            });
            rects.push(("group1".to_string(), r1.response.rect));
            let r2 = ButtonGroup::show(toolbar_ui, |group_ui| {
                for icon in [LucideIcon::Undo2, LucideIcon::Redo2] {
                    let _ = Button::icon_only(icon).show(group_ui);
                }
            });
            rects.push(("group2".to_string(), r2.response.rect));
        });
    });
    output.textures_delta.clear();
    let g1 = rects[0].1;
    let g2 = rects[1].1;
    assert!(
        (g1.center().y - g2.center().y).abs() < 0.5,
        "wrap false groups not aligned"
    );
}

#[test]
fn toolbar_gap_without_separators() {
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(800.0, 600.0))),
        ..Default::default()
    };
    let mut rects: Vec<(String, Rect)> = Vec::new();
    let mut output = ctx.run_ui(raw, |ui| {
        let _ = Toolbar::new().show(ui, |toolbar_ui| {
            let r1 = ButtonGroup::show(toolbar_ui, |group_ui| {
                for icon in [
                    LucideIcon::MousePointer2,
                    LucideIcon::PenTool,
                    LucideIcon::Spline,
                ] {
                    let _ = Button::icon_only(icon).show(group_ui);
                }
            });
            rects.push(("group1".to_string(), r1.response.rect));
            let r2 = ButtonGroup::show(toolbar_ui, |group_ui| {
                let _ = Button::icon_only(LucideIcon::Undo2).show(group_ui);
                let _ = Button::icon_only(LucideIcon::Redo2).show(group_ui);
            });
            rects.push(("group2".to_string(), r2.response.rect));
            let b1 = Button::new("Snap").show(toolbar_ui);
            rects.push(("snap".to_string(), b1.rect));
            let b2 = Button::new("Preview")
                .icon(LucideIcon::Play)
                .show(toolbar_ui);
            rects.push(("preview".to_string(), b2.rect));
        });
    });
    output.textures_delta.clear();
    let g1 = rects[0].1;
    let g2 = rects[1].1;
    let snap = rects[2].1;
    // Gap should be at least item_spacing (6) - allow 0.5 tolerance
    let gap1 = g2.min.x - g1.max.x;
    let gap2 = snap.min.x - g2.max.x;
    assert!(
        gap1 >= 5.5,
        "gap between group1 and group2 too small: {gap1} g1 {g1:?} g2 {g2:?}"
    );
    assert!(gap2 >= 5.5, "gap between group2 and snap too small: {gap2}");
    // Also check vertical alignment still
    assert!(
        (g1.center().y - g2.center().y).abs() < 0.5,
        "groups not aligned without separators"
    );
    assert!(
        (snap.center().y - g1.center().y).abs() < 0.5,
        "snap not aligned"
    );
}

#[test]
fn toolbar_vertical_alignment_dense() {
    let ctx = Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(800.0, 600.0))),
        ..Default::default()
    };
    let mut rects: Vec<(String, Rect)> = Vec::new();
    let mut output = ctx.run_ui(raw, |ui| {
        let _ = Toolbar::new().dense().show(ui, |toolbar_ui| {
            let r1 = ButtonGroup::show(toolbar_ui, |group_ui| {
                for icon in [LucideIcon::MousePointer2, LucideIcon::PenTool] {
                    let _ = Button::icon_only(icon)
                        .size(functora_egui::ComponentSize::Sm)
                        .show(group_ui);
                }
            });
            rects.push(("group1".to_string(), r1.response.rect));
            let r2 = ButtonGroup::show(toolbar_ui, |group_ui| {
                let _ = Button::icon_only(LucideIcon::Undo2)
                    .size(functora_egui::ComponentSize::Sm)
                    .show(group_ui);
            });
            rects.push(("group2".to_string(), r2.response.rect));
            let s = Separator::vertical().show(toolbar_ui);
            rects.push(("sep".to_string(), s.rect));
            let b = Button::new("Snap")
                .size(functora_egui::ComponentSize::Sm)
                .show(toolbar_ui);
            rects.push(("snap".to_string(), b.rect));
        });
    });
    output.textures_delta.clear();
    let g1 = rects[0].1;
    let g2 = rects[1].1;
    let sep = rects[2].1;
    let snap = rects[3].1;
    assert!(
        (g1.center().y - g2.center().y).abs() < 0.5,
        "dense groups not aligned"
    );
    assert!(
        (sep.center().y - g1.center().y).abs() < 0.5,
        "dense sep not centered"
    );
    assert!(
        (snap.center().y - g1.center().y).abs() < 0.5,
        "dense button not aligned"
    );
}
