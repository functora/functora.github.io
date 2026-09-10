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
        .find(|r| {
            r.width() > 40.0
                && r.width() < 280.0
                && r.height() >= 24.0
                && r.height() <= 50.0
                && r.top() > 30.0
        })
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

#[test]
fn tooltip_inside_flex_shows_on_hover() {
    use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
    use functora_egui::{Button, ButtonVariant, ComponentSize, Flex, LucideIcon, Tooltip};
    use std::cell::Cell;
    use std::rc::Rc;

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

        fn texts(output: &egui::FullOutput) -> Vec<String> {
            output
                .shapes
                .iter()
                .filter_map(|cs| match &cs.shape {
                    Shape::Text(ts) => Some(ts.galley.text().to_owned()),
                    _ => None,
                })
                .collect()
        }
    }

    let rect_cell = Rc::new(Cell::new(None::<Rect>));
    let rect_cell_clone = rect_cell.clone();
    let mut body = move |ui: &mut egui::Ui| {
        let _ = Flex::row().gap(8.0).show(ui, |f| {
            let settings = f.add(
                Button::icon_only(LucideIcon::Settings)
                    .variant(ButtonVariant::Outline)
                    .size(ComponentSize::Sm),
            );
            rect_cell_clone.set(Some(settings.inner.rect));
            Tooltip::new("Settings").show(&settings.inner);
        });
    };

    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let rect = rect_cell.get().expect("button rect not found");
    let center = rect.center();
    let _ = app.step(vec![Event::PointerMoved(center)], &mut body);
    let mut found = false;
    for _ in 0..60 {
        let out = app.step(vec![], &mut body);
        if App::texts(&out).iter().any(|t| t.contains("Settings")) {
            found = true;
            break;
        }
    }
    assert!(found, "tooltip inside Flex should appear on hover");
}

#[test]
fn tooltip_direct_shows_on_hover() {
    use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
    use functora_egui::{Button, ButtonVariant, ComponentSize, LucideIcon, Tooltip};

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

        fn texts(output: &egui::FullOutput) -> Vec<String> {
            output
                .shapes
                .iter()
                .filter_map(|cs| match &cs.shape {
                    Shape::Text(ts) => Some(ts.galley.text().to_owned()),
                    _ => None,
                })
                .collect()
        }
    }

    let rect_cell = std::rc::Rc::new(std::cell::Cell::new(None::<Rect>));
    let rect_cell_clone = rect_cell.clone();
    let mut body = move |ui: &mut egui::Ui| {
        let resp = Button::icon_only(LucideIcon::Settings)
            .variant(ButtonVariant::Outline)
            .size(ComponentSize::Sm)
            .show(ui);
        rect_cell_clone.set(Some(resp.rect));
        Tooltip::new("DirectTip").show(&resp);
    };

    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let rect = rect_cell.get().expect("button rect");
    let center = rect.center();
    let _ = app.step(vec![Event::PointerMoved(center)], &mut body);
    let mut found = false;
    for _ in 0..60 {
        let out = app.step(vec![], &mut body);
        if App::texts(&out).iter().any(|t| t.contains("DirectTip")) {
            found = true;
            break;
        }
    }
    assert!(found, "direct tooltip should appear on hover");
}

#[test]
fn spinner_widget_spins() {
    use egui::{Context, Pos2, RawInput, Rect, Vec2};
    use functora_egui::Spinner;

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

        fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
            self.frame += 1;
            let raw = RawInput {
                screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
                time: Some(f64::from(self.frame) / 60.0),
                ..Default::default()
            };
            let mut out = self.ctx.run_ui(raw, |ui| {
                let _ = egui::CentralPanel::default().show(ui, |inner_ui| body(inner_ui));
            });
            out.textures_delta.clear();
            out
        }

        fn shapes_key(output: &egui::FullOutput) -> String {
            format!("{:?}", output.shapes)
        }
    }

    let mut body = |ui: &mut egui::Ui| {
        let _ = Spinner::new().size(24.0).show(ui);
    };
    let mut app = App::new();
    let out1 = app.step(&mut body);
    for _ in 0..10 {
        let _ = app.step(&mut body);
    }
    let out2 = app.step(&mut body);
    assert_ne!(
        App::shapes_key(&out1),
        App::shapes_key(&out2),
        "Spinner should animate over time"
    );
}

#[test]
fn button_loader_icon_spins_and_non_loader_is_static() {
    use egui::{Context, Pos2, RawInput, Rect, Vec2};
    use functora_egui::{Button, Flex, LucideIcon};

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

        fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
            self.frame += 1;
            let raw = RawInput {
                screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
                time: Some(f64::from(self.frame) / 60.0),
                ..Default::default()
            };
            let mut out = self.ctx.run_ui(raw, |ui| {
                let _ = egui::CentralPanel::default().show(ui, |inner_ui| body(inner_ui));
            });
            out.textures_delta.clear();
            out
        }

        fn shapes_key(output: &egui::FullOutput) -> String {
            format!("{:?}", output.shapes)
        }
    }

    let mut loader_body = |ui: &mut egui::Ui| {
        let _ = Flex::row().gap(8.0).show(ui, |f| {
            let _ = f.add(
                Button::new("Loading")
                    .icon(LucideIcon::LoaderCircle)
                    .enabled(false),
            );
        });
    };
    let mut app = App::new();
    let out1 = app.step(&mut loader_body);
    for _ in 0..10 {
        let _ = app.step(&mut loader_body);
    }
    let out2 = app.step(&mut loader_body);
    assert_ne!(
        App::shapes_key(&out1),
        App::shapes_key(&out2),
        "Button with LoaderCircle should animate"
    );

    let mut static_body = |ui: &mut egui::Ui| {
        let _ = Button::new("Save").icon(LucideIcon::Save).show(ui);
    };
    let mut app2 = App::new();
    let out1_static = app2.step(&mut static_body);
    for _ in 0..10 {
        let _ = app2.step(&mut static_body);
    }
    let out2_static = app2.step(&mut static_body);
    assert_eq!(
        App::shapes_key(&out1_static),
        App::shapes_key(&out2_static),
        "Button with non-loader icon should be static"
    );
}

#[test]
fn toast_buttons_inside_flex_trigger_toast() {
    use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
    use functora_egui::{Button, ButtonVariant, Flex, ToastState, ToastVariant};
    use std::cell::RefCell;
    use std::rc::Rc;

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

        fn has_toast_text(output: &egui::FullOutput, needle: &str) -> bool {
            output.shapes.iter().any(|cs| match &cs.shape {
                Shape::Text(ts) => ts.galley.text().contains(needle),
                _ => false,
            })
        }
    }

    let toast_state = Rc::new(RefCell::new(ToastState::new()));
    let toast_state_clone = toast_state.clone();
    let mut body = move |ui: &mut egui::Ui| {
        let ctx = ui.ctx().clone();
        let mut guard = toast_state_clone.borrow_mut();
        let _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            if f.add(Button::new("Default").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                guard.add(
                    "Default toast",
                    ToastVariant::Default,
                    ctx.input(|i| i.time),
                );
            }
            if f.add(Button::new("Success").variant(ButtonVariant::Outline))
                .inner
                .clicked()
            {
                guard.add(
                    "Success toast",
                    ToastVariant::Success,
                    ctx.input(|i| i.time),
                );
            }
        });
        guard.show(ui.ctx());
    };

    let mut app = App::new();
    let out_initial = app.step(vec![], &mut body);
    let button_rect = App::rects(&out_initial)
        .into_iter()
        .find(|r| r.width() > 40.0 && r.width() < 150.0 && r.height() > 20.0 && r.height() < 50.0)
        .expect("toast button rect not found");
    let center = button_rect.center();
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
    let out_final = app.step(vec![], &mut body);
    assert!(
        App::has_toast_text(&out_final, "Default toast"),
        "toast inside Flex should be triggered on click via inner response"
    );
}

#[test]
fn toast_vertical_spacing_is_consistent() {
    use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};
    use functora_egui::{ToastState, ToastVariant};

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

        fn step(&mut self, toast: &mut ToastState) -> egui::FullOutput {
            self.frame += 1;
            let raw = RawInput {
                screen_rect: Some(Rect::from_min_size(Pos2::ZERO, SCREEN)),
                time: Some(f64::from(self.frame) / 60.0),
                ..Default::default()
            };
            let mut out = self.ctx.run_ui(raw, |ui| {
                toast.show(ui.ctx());
            });
            out.textures_delta.clear();
            out
        }

        fn toast_rects(output: &egui::FullOutput) -> Vec<Rect> {
            output
                .shapes
                .iter()
                .filter_map(|cs| {
                    let rect = match &cs.shape {
                        Shape::Rect(rect_shape) => rect_shape.rect,
                        Shape::Vec(vec) => vec
                            .iter()
                            .find_map(|shape| match shape {
                                Shape::Rect(rect) => Some(rect.rect),
                                _ => None,
                            })
                            .unwrap_or(egui::Rect::NOTHING),
                        _ => return None,
                    };
                    if (350.0..400.0).contains(&rect.width()) && rect.height() > 30.0 {
                        Some(rect)
                    } else {
                        None
                    }
                })
                .collect()
        }
    }

    let assert_spacing = |mut toast: ToastState| {
        let mut app = App::new();
        let _ = app.step(&mut toast);
        let out = app.step(&mut toast);
        let mut rects = App::toast_rects(&out);
        assert_eq!(rects.len(), 2, "expected 2 toasts, found {rects:?}");
        rects.sort_by(|left, right| right.max.y.partial_cmp(&left.max.y).unwrap());
        let bottom = rects[0];
        let top = rects[1];
        let gap = bottom.min.y - top.max.y;
        assert!(
            (gap - 8.0).abs() < 1.0,
            "toast gap should be 8.0, got {gap:.2} for rects {rects:?}"
        );
    };

    let mut with_desc_first = ToastState::new();
    with_desc_first.add("Default toast", ToastVariant::Default, 0.0);
    with_desc_first.add_with_description(
        "With desc",
        "Friday, February 10, 2026 at 5:57 PM",
        ToastVariant::Default,
        0.0,
    );
    assert_spacing(with_desc_first);

    let mut desc_first = ToastState::new();
    desc_first.add_with_description(
        "With desc",
        "Friday, February 10, 2026 at 5:57 PM",
        ToastVariant::Default,
        0.0,
    );
    desc_first.add("Default toast", ToastVariant::Default, 0.0);
    assert_spacing(desc_first);

    let mut two_defaults = ToastState::new();
    two_defaults.add("Default1", ToastVariant::Default, 0.0);
    two_defaults.add("Default2", ToastVariant::Default, 0.0);
    assert_spacing(two_defaults);

    let mut two_descs = ToastState::new();
    two_descs.add_with_description(
        "Desc1",
        "Friday, February 10, 2026 at 5:57 PM",
        ToastVariant::Default,
        0.0,
    );
    two_descs.add_with_description(
        "Desc2",
        "Friday, February 10, 2026 at 5:57 PM",
        ToastVariant::Default,
        0.0,
    );
    assert_spacing(two_descs);

    let multiline_desc = "Uploaded 128 files, skipped 3 files, and downloaded 42 files in the background. Next sync happens automatically when the device is back online.";

    let mut multiline_first = ToastState::new();
    multiline_first.add_with_description("Sync report", multiline_desc, ToastVariant::Success, 0.0);
    multiline_first.add("Default toast", ToastVariant::Default, 0.0);
    assert_spacing(multiline_first);

    let mut multiline_last = ToastState::new();
    multiline_last.add("Default toast", ToastVariant::Default, 0.0);
    multiline_last.add_with_description("Sync report", multiline_desc, ToastVariant::Success, 0.0);
    assert_spacing(multiline_last);

    let mut long_title = ToastState::new();
    long_title.add(
        "Sync completed with a very long multiline title that wraps across several lines",
        ToastVariant::Success,
        0.0,
    );
    long_title.add("Default toast", ToastVariant::Default, 0.0);
    let mut app = App::new();
    let _ = app.step(&mut long_title);
    let out = app.step(&mut long_title);
    let mut rects = App::toast_rects(&out);
    assert_eq!(rects.len(), 2, "expected 2 toasts, found {rects:?}");
    for rect in &rects {
        assert!(
            rect.width() <= 390.5,
            "toast must not exceed max outer width 388.0 + 1px stroke, got {rect:?}"
        );
        assert!(
            rect.max.x <= 1280.0 - 15.0,
            "toast must keep right margin, got {rect:?}"
        );
    }
    rects.sort_by(|left, right| right.max.y.partial_cmp(&left.max.y).unwrap());
    let gap = rects[0].min.y - rects[1].max.y;
    assert!(
        (gap - 8.0).abs() < 1.0,
        "toast gap should be 8.0, got {gap:.2} for rects {rects:?}"
    );
}

#[test]
fn image_builder_sources_construct_without_panic() {
    let _img = egui::Image::from_bytes("bytes://test", vec![0u8; 4]);
    let _uri = egui::Image::from_uri("file:///tmp/x.png");
}

#[test]
fn image_builder_options_render_without_panic() {
    let mut app = App::new();
    let _ = app.step(vec![], &mut |ui| {
        let _ = ui.add(
            egui::Image::from_bytes("bytes://a", vec![])
                .bg_fill(egui::Color32::RED)
                .tint(egui::Color32::BLUE)
                .alt_text("missing")
                .maintain_aspect_ratio(true)
                .fit_to_exact_size(egui::vec2(200.0, 120.0)),
        );
    });
}

#[cfg(feature = "images")]
fn image_samples() -> &'static functora_egui::ImageSamples {
    match functora_egui::image_samples() {
        Ok(samples) => samples,
        Err(error) => panic!("sample fixtures must encode: {error:?}"),
    }
}

#[test]
#[cfg(feature = "images")]
fn image_sample_fixtures_encode_successfully() {
    let samples = image_samples();
    for (label, expected_ext, sample) in [
        ("svg", ".svg", &samples.svg),
        ("png", ".png", &samples.png),
        ("jpeg", ".jpg", &samples.jpeg),
        ("gif", ".gif", &samples.gif),
        ("webp", ".webp", &samples.webp),
        ("bmp", ".bmp", &samples.bmp),
        ("ico", ".ico", &samples.ico),
        ("qoi", ".qoi", &samples.qoi),
        ("farbfeld", ".ff", &samples.farbfeld),
        ("tiff", ".tiff", &samples.tiff),
        ("ppm", ".ppm", &samples.pnm),
        ("transparent", ".png", &samples.transparent),
    ] {
        assert!(
            !sample.bytes.is_empty(),
            "{label} fixture must not be empty"
        );
        assert!(
            sample.uri().ends_with(expected_ext),
            "{label} uri must end with {expected_ext} for loader routing, got {}",
            sample.uri(),
        );
    }
}

#[test]
#[cfg(feature = "images")]
fn image_sample_fixtures_decode_successfully() {
    let samples = image_samples();
    for (label, sample) in [
        ("png", &samples.png),
        ("jpeg", &samples.jpeg),
        ("gif", &samples.gif),
        ("webp", &samples.webp),
        ("bmp", &samples.bmp),
        ("ico", &samples.ico),
        ("qoi", &samples.qoi),
        ("farbfeld", &samples.farbfeld),
        ("tiff", &samples.tiff),
        ("ppm", &samples.pnm),
        ("transparent", &samples.transparent),
    ] {
        match image::load_from_memory(&sample.bytes) {
            Ok(decoded) => assert_eq!(
                (decoded.width(), decoded.height()),
                (48, 48),
                "{label} fixture must decode to 48x48",
            ),
            Err(error) => panic!("{label} fixture must decode: {error:?}"),
        }
    }
    match std::str::from_utf8(&samples.svg.bytes) {
        Ok(text) => assert!(
            text.starts_with("<svg") && text.ends_with("</svg>"),
            "svg fixture must be an svg document",
        ),
        Err(error) => panic!("svg fixture must be utf-8: {error:?}"),
    }
}

#[test]
#[cfg(feature = "images")]
fn image_fit_variants_render_without_panic() {
    let bytes = image_samples().png.bytes.clone();
    let mut app = App::new();
    let _ = app.step(vec![], &mut |ui| {
        let _ = ui.add(
            egui::Image::from_bytes("bytes://f-contain", bytes.clone())
                .maintain_aspect_ratio(true)
                .max_width(200.0)
                .max_height(120.0),
        );
        let _ = ui.add(
            egui::Image::from_bytes("bytes://f-cover", bytes.clone())
                .maintain_aspect_ratio(true)
                .fit_to_exact_size(egui::vec2(200.0, 120.0)),
        );
        let _ = ui.add(
            egui::Image::from_bytes("bytes://f-width", bytes.clone())
                .maintain_aspect_ratio(true)
                .max_width(200.0),
        );
        let _ = ui.add(
            egui::Image::from_bytes("bytes://f-fill", bytes.clone())
                .fit_to_exact_size(egui::vec2(200.0, 120.0)),
        );
    });
}

#[test]
#[cfg(feature = "images")]
fn image_all_formats_render_without_panic() {
    let samples = image_samples();
    let formats: [&functora_egui::ImageBytes; 11] = [
        &samples.svg,
        &samples.png,
        &samples.jpeg,
        &samples.gif,
        &samples.webp,
        &samples.bmp,
        &samples.ico,
        &samples.qoi,
        &samples.farbfeld,
        &samples.tiff,
        &samples.pnm,
    ];
    let mut app = App::new();
    for sample in formats {
        let _ = app.step(vec![], &mut |ui| {
            let _ = ui.add(sample.to_image().max_width(96.0).alt_text(sample.uri()));
        });
    }
}

#[test]
#[cfg(feature = "images")]
fn image_to_image_preserves_uri() {
    let samples = image_samples();
    for sample in [
        &samples.svg,
        &samples.png,
        &samples.jpeg,
        &samples.gif,
        &samples.webp,
        &samples.bmp,
        &samples.ico,
        &samples.qoi,
        &samples.farbfeld,
        &samples.tiff,
        &samples.pnm,
        &samples.transparent,
    ] {
        assert_eq!(sample.to_image().uri(), Some(sample.uri().as_str()));
    }
}

#[test]
#[cfg(feature = "images")]
fn image_unbounded_render_without_panic() {
    let bytes = image_samples().png.bytes.clone();
    let mut app = App::new();
    let _ = app.step(vec![], &mut |ui| {
        let _ = ui.add(
            egui::Image::from_bytes("bytes://t-bounds", bytes.clone()).maintain_aspect_ratio(true),
        );
    });
}

#[test]
fn image_corrupt_bytes_render_without_panic() {
    let mut app = App::new();
    let _ = app.step(vec![], &mut |ui| {
        let _ = ui.add(
            egui::Image::from_bytes("bytes://t-corrupt", b"corrupt".to_vec())
                .alt_text("Image unavailable")
                .max_width(120.0),
        );
    });
}

#[test]
#[cfg(feature = "images")]
fn image_renders_non_empty_rect() {
    let bytes = image_samples().png.bytes.clone();
    let mut app = App::new();
    let out = app.step(vec![], &mut |ui| {
        let _ = ui.add(egui::Image::from_bytes("bytes://demo", bytes.clone()).max_width(50.0));
    });
    let rects = App::rects(&out);
    assert!(
        rects.iter().any(|r| r.width() > 1.0 && r.height() > 1.0),
        "image should paint a sizable rect, got {rects:?}"
    );
}

#[test]
#[cfg(feature = "images")]
fn image_sense_click_detected() {
    let bytes = image_samples().png.bytes.clone();
    let mut app = App::new();
    let mut clicked = false;
    let mut body = |ui: &mut egui::Ui| {
        if ui
            .add(
                egui::Image::from_bytes("bytes://c", bytes.clone())
                    .sense(egui::Sense::click())
                    .max_width(50.0),
            )
            .clicked()
        {
            clicked = true;
        }
    };
    // First frame: establish the image rect under the cursor.
    let _ = app.step(vec![], &mut body);
    // Press, then release on the image to trigger a click.
    let press = vec![egui::Event::PointerButton {
        pos: Pos2::new(25.0, 25.0),
        button: egui::PointerButton::Primary,
        pressed: true,
        modifiers: egui::Modifiers::default(),
    }];
    let _ = app.step(press, &mut body);
    let release = vec![egui::Event::PointerButton {
        pos: Pos2::new(25.0, 25.0),
        button: egui::PointerButton::Primary,
        pressed: false,
        modifiers: egui::Modifiers::default(),
    }];
    let _ = app.step(release, &mut body);
    assert!(
        clicked,
        "press+release on the image should register a click"
    );
}
