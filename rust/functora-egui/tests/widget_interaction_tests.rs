//! Interaction tests: pointer drag on Resizable, wheel scroll on ScrollArea.

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

struct App {
    ctx: Context,
    frame: usize,
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
            time: Some(self.frame as f64 / 60.0),
            events,
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            egui::CentralPanel::default().show(ui, |ui| body(ui));
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
    app.step(
        vec![Event::PointerButton {
            pos: from,
            button: egui::PointerButton::Primary,
            pressed: true,
            modifiers: Default::default(),
        }],
        body,
    );
    let steps = 8;
    for i in 1..=steps {
        let t = i as f32 / steps as f32;
        let pos = Pos2::new(from.x + (to.x - from.x) * t, from.y + (to.y - from.y) * t);
        app.step(vec![Event::PointerMoved(pos)], body);
    }
    app.step(
        vec![Event::PointerButton {
            pos: to,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: Default::default(),
        }],
        body,
    );
}

#[test]
fn resizable_handle_drag_changes_fraction() {
    let mut fraction = 0.5_f32;
    let mut body = |ui: &mut egui::Ui| {
        functora_egui::Resizable::new().height(240.0).show(
            ui,
            &mut fraction,
            |ui| {
                ui.label("L");
            },
            |ui| {
                ui.label("R");
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
        egui::ScrollArea::vertical()
            .auto_shrink([false; 2])
            .show(ui, |ui| {
                ui.add_space(spacing.page_padding);
                ui.horizontal(|ui| {
                    ui.add_space(margin);
                    ui.add_space(spacing.page_padding);
                    ui.vertical(|ui| {
                        ui.set_max_width(inner_width);
                        functora_egui::Typography::muted("Draggable split pane.").show(ui);
                        ui.add_space(12.0);
                        functora_egui::Resizable::new().height(160.0).show(
                            ui,
                            &mut fraction,
                            |ui| {
                                ui.label("L");
                            },
                            |ui| {
                                ui.label("R");
                            },
                        );
                        reported = format!(
                            "ui_max={:?} avail_before_desc={} inner={inner_width} margin={margin}",
                            ui.max_rect(),
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
        functora_egui::Command::new(items.clone()).show(ui.ctx(), &mut open, &mut search);
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
    app.step(vec![], &mut body);
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

    app.step(
        vec![Event::PointerMoved(egui::pos2(640.0, 400.0))],
        &mut body,
    );
    for _ in 0..20 {
        app.step(
            vec![Event::MouseWheel {
                unit: egui::MouseWheelUnit::Point,
                delta: Vec2::new(0.0, -100.0),
                modifiers: Default::default(),
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
        functora_egui::ScrollArea::new(200.0).show(ui, |ui| {
            let (rect, _) = ui
                .allocate_exact_size(egui::vec2(ui.available_width(), 20.0), egui::Sense::hover());
            ui.painter()
                .rect_filled(rect, egui::CornerRadius::ZERO, egui::Color32::RED);
            for i in 0..40 {
                ui.label(format!("row {i}"));
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
    app.step(vec![Event::PointerMoved(hover)], &mut body);

    for _ in 0..5 {
        app.step(
            vec![Event::MouseWheel {
                unit: egui::MouseWheelUnit::Point,
                delta: Vec2::new(0.0, -50.0),
                modifiers: Default::default(),
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
        _ = egui::Panel::top("top_bar").show(ui, |ui| {
            ui.label("top");
        });
        _ = egui::Panel::right("sidebar_panel")
            .default_size(244.0)
            .show(ui, |ui| {
                Sidebar::new()
                    .width(228.0)
                    .collapsible()
                    .show(ui, &mut collapsed, |ui| {
                        if ui
                            .add(
                                Button::new("Checkbox")
                                    .variant(ButtonVariant::Default)
                                    .full_width(),
                            )
                            .clicked()
                        {
                            clicked = true;
                        }
                    });
            });
        _ = egui::CentralPanel::default().show(ui, |ui| {
            ui.label("central");
        });
    };

    let mut app = App::new();
    app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let button = App::rects(&out)
        .into_iter()
        .find(|r| r.width() > 40.0 && r.width() < 280.0 && r.height() < 30.0 && r.top() > 30.0)
        .expect("nav button rect not found");

    let center = button.center();
    app.step(vec![Event::PointerMoved(center)], &mut body);
    app.step(
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Primary,
            pressed: true,
            modifiers: Default::default(),
        }],
        &mut body,
    );
    app.step(
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: Default::default(),
        }],
        &mut body,
    );

    assert!(clicked, "click on sidebar nav item did not register");
}
