#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Interaction tests: pointer drag on Resizable, wheel scroll on `ScrollArea`.

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

struct App {
    ctx: Context,
    frame: u32,
}

impl App {
    fn new() -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
        }
    }

    fn step(
        &mut self,
        events: Vec<Event>,
        body: &mut dyn FnMut(&mut egui::Ui),
    ) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner_ui| body(inner_ui));
        });
        out.textures_delta.clear();
        out
    }

    fn rects(output: &egui::FullOutput) -> Vec<Rect> {
        output
            .shapes
            .iter()
            .filter_map(|cs| match &cs.shape {
                Shape::Rect(rs) => Some(rs.rect),
                Shape::Vec(v) => v.iter().find_map(|s| match s {
                    Shape::Rect(rs) => Some(rs.rect),
                    _ => None,
                }),
                _ => None,
            })
            .collect()
    }
}

fn drag(app: &mut App, from: Pos2, to: Pos2, body: &mut dyn FnMut(&mut egui::Ui)) {
    let _ = app.step(
        vec![Event::PointerButton {
            pos: from,
            button: egui::PointerButton::Primary,
            pressed: true,
            modifiers: egui::Modifiers::default(),
        }],
        body,
    );
    let steps: u8 = 8;
    for idx in 1..=steps {
        let t = f32::from(idx) / f32::from(steps);
        let pos = Pos2::new(from.x + (to.x - from.x) * t, from.y + (to.y - from.y) * t);
        let _ = app.step(vec![Event::PointerMoved(pos)], body);
    }
    let _ = app.step(
        vec![Event::PointerButton {
            pos: to,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
        body,
    );
}

#[test]
fn resizable_handle_drag_changes_fraction() {
    let mut fraction = 0.5_f32;
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::Resizable::new().height(240.0).show(
            ui,
            &mut fraction,
            |left_ui| {
                let _ = left_ui.label("L");
            },
            |right_ui| {
                let _ = right_ui.label("R");
            },
        );
    };

    let mut app = App::new();
    let out = app.step(vec![], &mut body);
    let handle = App::rects(&out)
        .into_iter()
        .find(|r| r.width() < 20.0 && r.height() > 100.0)
        .expect("handle rect not found");

    let center = handle.center();
    drag(
        &mut app,
        center,
        Pos2::new(center.x + 90.0, center.y),
        &mut body,
    );

    assert!(
        (fraction - 0.5).abs() > 0.01,
        "fraction did not change after dragging the handle: {fraction}"
    );
}

#[test]
fn resizable_inside_demo_shell_has_full_width() {
    let mut fraction = 0.5_f32;
    let mut reported = String::new();
    let mut body = |ui: &mut egui::Ui| {
        let spacing = functora_egui::ResponsiveExt::responsive_spacing(ui);
        let available = ui.available_width();
        let content_width = available.min(spacing.content_max_width);
        let margin = ((available - content_width) * 0.5).max(0.0);
        let inner_width = (content_width - 2.0 * spacing.page_padding).max(0.0);
        let _ = egui::ScrollArea::vertical()
            .auto_shrink([false; 2])
            .show(ui, |scroll_ui| {
                scroll_ui.add_space(spacing.page_padding);
                let _ = scroll_ui.horizontal(|row_ui| {
                    row_ui.add_space(margin);
                    row_ui.add_space(spacing.page_padding);
                    let _ = row_ui.vertical(|col_ui| {
                        col_ui.set_max_width(inner_width);
                        let _ =
                            functora_egui::Typography::muted("Draggable split pane.").show(col_ui);
                        col_ui.add_space(12.0);
                        let _ = functora_egui::Resizable::new().height(160.0).show(
                            col_ui,
                            &mut fraction,
                            |left_ui| {
                                let _ = left_ui.label("L");
                            },
                            |right_ui| {
                                let _ = right_ui.label("R");
                            },
                        );
                        reported = format!(
                            "ui_max={:?} avail_before_desc={} inner={inner_width} margin={margin}",
                            col_ui.max_rect(),
                            available,
                        );
                    });
                });
            });
    };

    let mut app = App::new();
    let out = app.step(vec![], &mut body);
    let handle = App::rects(&out)
        .into_iter()
        .find(|r| r.width() < 20.0 && r.height() > 100.0)
        .expect("handle rect not found");
    println!("DEBUG: {reported}");
    println!("DEBUG: handle={handle:?} fraction={fraction}");
    assert!(
        handle.width() >= 8.0 && handle.left() > 400.0,
        "handle should sit near the middle of a full-width panel, got {handle:?}"
    );
}

#[test]
fn command_palette_list_scrolls_when_overflowing() {
    let items: Vec<(String, String)> = (0..40)
        .map(|i| ("Group".to_owned(), format!("item {i}")))
        .collect();
    let mut open = true;
    let mut search = String::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::Command::new(items.clone()).show(ui.ctx(), &mut open, &mut search);
    };

    let visible_texts = |output: &egui::FullOutput| -> std::collections::HashSet<String> {
        output
            .shapes
            .iter()
            .filter_map(|cs| match &cs.shape {
                Shape::Text(ts) if (300.0..400.0).contains(&cs.clip_rect.height()) => {
                    Some(ts.galley.text().to_owned())
                }
                _ => None,
            })
            .collect()
    };
    let list_overflowing = |output: &egui::FullOutput| -> bool {
        output.shapes.iter().any(|cs| {
            (300.0..400.0).contains(&cs.clip_rect.height())
                && matches!(&cs.shape, Shape::Rect(rs) if rs.rect.height() > 400.0)
        })
    };

    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let before = app.step(vec![], &mut body);
    assert!(
        list_overflowing(&before),
        "palette list should overflow its capped height"
    );
    let before_texts = visible_texts(&before);
    assert!(
        !before_texts.iter().any(|t| t.contains("item 39")),
        "last item should be out of view before scrolling"
    );

    let _ = app.step(
        vec![Event::PointerMoved(egui::pos2(640.0, 400.0))],
        &mut body,
    );
    for _ in 0..20 {
        let _ = app.step(
            vec![Event::MouseWheel {
                unit: egui::MouseWheelUnit::Point,
                delta: Vec2::new(0.0, -100.0),
                modifiers: egui::Modifiers::default(),
                phase: egui::TouchPhase::Move,
            }],
            &mut body,
        );
    }
    let _ = app.step(vec![], &mut body);
    let _ = app.step(vec![], &mut body);

    let after = app.step(vec![], &mut body);
    let after_texts = visible_texts(&after);
    assert!(
        after_texts.iter().any(|t| t.contains("item 39")),
        "last item should come into view after scrolling: {after_texts:?}"
    );
}

#[test]
fn scroll_area_wheel_scrolls_content() {
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::ScrollArea::new(200.0).show(ui, |scroll_ui| {
            let (rect, _) = scroll_ui.allocate_exact_size(
                egui::vec2(scroll_ui.available_width(), 20.0),
                egui::Sense::hover(),
            );
            let _ =
                scroll_ui
                    .painter()
                    .rect_filled(rect, egui::CornerRadius::ZERO, egui::Color32::RED);
            for idx in 0..40 {
                let _ = scroll_ui.label(format!("row {idx}"));
            }
        });
    };

    let mut app = App::new();
    let before = app.step(vec![], &mut body);
    let marker_before = App::rects(&before)
        .into_iter()
        .find(|r| r.height() < 25.0 && r.top() < 400.0)
        .expect("marker rect not found");

    let hover = marker_before.center();
    let _ = app.step(vec![Event::PointerMoved(hover)], &mut body);

    for _ in 0..5 {
        let _ = app.step(
            vec![Event::MouseWheel {
                unit: egui::MouseWheelUnit::Point,
                delta: Vec2::new(0.0, -50.0),
                modifiers: egui::Modifiers::default(),
                phase: egui::TouchPhase::Move,
            }],
            &mut body,
        );
    }

    let after = app.step(vec![], &mut body);
    let marker_after = App::rects(&after)
        .into_iter()
        .find(|r| r.height() < 25.0 && r.top() < 400.0)
        .expect("marker rect not found");

    assert!(
        (marker_after.top() - marker_before.top()).abs() > 10.0,
        "content did not move after wheel events: before={marker_before:?} after={marker_after:?}"
    );
}

#[test]
fn sidebar_item_click_registers() {
    use functora_egui::{Button, ButtonVariant, Sidebar};

    let mut collapsed = false;
    let mut clicked = false;
    let mut body = |ui: &mut egui::Ui| {
        _ = egui::Panel::top("top_bar").show(ui, |top_ui| {
            let _ = top_ui.label("top");
        });
        _ = egui::Panel::right("sidebar_panel")
            .default_size(244.0)
            .show(ui, |sidebar_ui| {
                let _ = Sidebar::new().width(228.0).collapsible().show(
                    sidebar_ui,
                    &mut collapsed,
                    |content_ui| {
                        if content_ui
                            .add(
                                Button::new("Checkbox")
                                    .variant(ButtonVariant::Default)
                                    .full_width(),
                            )
                            .clicked()
                        {
                            clicked = true;
                        }
                    },
                );
            });
        _ = egui::CentralPanel::default().show(ui, |central_ui| {
            let _ = central_ui.label("central");
        });
    };

    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let button = App::rects(&out)
        .into_iter()
        .find(|r| r.width() > 40.0 && r.width() < 280.0 && r.height() < 30.0 && r.top() > 30.0)
        .expect("nav button rect not found");

    let center = button.center();
    let _ = app.step(vec![Event::PointerMoved(center)], &mut body);
    let _ = app.step(
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Primary,
            pressed: true,
            modifiers: egui::Modifiers::default(),
        }],
        &mut body,
    );
    let _ = app.step(
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
        &mut body,
    );

    assert!(clicked, "click on sidebar nav item did not register");
}
