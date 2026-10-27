//! Regression: text and widget boxes must never overflow their container.
//!
//! Reproduces the reported mobile overflow: long (e.g. Russian) tab titles
//! painted letter-by-letter outside their tab buttons, plus the same class of
//! bug in labels, fixed-width buttons and connected button groups.

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    Create,
    Open,
    Scan,
}

struct Harness {
    ctx: Context,
    frame: u32,
    screen: Vec2,
}

impl Harness {
    fn new(screen: Vec2) -> Self {
        Self {
            ctx: Context::default(),
            frame: 0,
            screen,
        }
    }

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, self.screen)),
            time: Some(f64::from(self.frame) / 60.0),
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }
}

fn text_rects(out: &egui::FullOutput) -> Vec<(Rect, String)> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Text(text) => Some((
                text.galley.rect.translate(text.pos.to_vec2()),
                text.galley.text().to_owned(),
            )),
            _ => None,
        })
        .collect()
}

fn box_rects(out: &egui::FullOutput) -> Vec<Rect> {
    out.shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect_shape)
                if rect_shape.fill.a() == 255
                    && (16.0..=64.0).contains(&rect_shape.rect.height())
                    && rect_shape.rect.width() >= 8.0 =>
            {
                Some(rect_shape.rect)
            }
            _ => None,
        })
        .collect()
}

fn assert_inside(inner: Rect, outer: Rect, what: &str) {
    assert!(
        outer.contains(inner.min) && outer.contains(inner.max),
        "{what} at {inner:?} escapes its container {outer:?}"
    );
}

fn assert_inside_x(inner: Rect, outer: Rect, what: &str) {
    assert!(
        outer.min.x <= inner.min.x && inner.max.x <= outer.max.x,
        "{what} at {inner:?} escapes its container horizontally {outer:?}"
    );
}

#[test]
fn tab_titles_stay_inside_their_tab_buttons() {
    let entries = [
        (Action::Create, "Создать заметку".to_owned()),
        (Action::Open, "Открыть заметку".to_owned()),
        (Action::Scan, "Сканировать заметку".to_owned()),
    ];
    let mut selected = Action::Create;
    let mut app = Harness::new(Vec2::new(360.0, 640.0));
    let mut body = |ui: &mut egui::Ui| {
        let _ =
            functora_egui::TabsValue::new(&entries)
                .fill_width()
                .show(ui, &mut selected, |_, _| {});
    };
    let out = app.step(&mut body);

    let boxes = box_rects(&out);
    assert_eq!(boxes.len(), 3, "tab bar must paint three tab boxes");
    for (rect, text) in text_rects(&out) {
        let host = boxes
            .iter()
            .find(|b| b.contains(rect.min) || b.contains(rect.max))
            .copied()
            .unwrap_or(Rect::NOTHING);
        assert!(
            host != Rect::NOTHING,
            "tab title {text:?} at {rect:?} is not inside any tab button {boxes:?}"
        );
        assert_inside(rect, host, &format!("tab title {text:?}"));
    }
}

#[test]
fn fill_width_tabs_stay_on_one_row_when_they_fit() {
    let entries = [
        (Action::Create, "Create".to_owned()),
        (Action::Open, "Open".to_owned()),
        (Action::Scan, "Scan".to_owned()),
    ];
    let mut selected = Action::Create;
    for width in [320.0_f32, 360.0, 400.0, 800.0] {
        let mut app = Harness::new(Vec2::new(width, 640.0));
        let mut body = |ui: &mut egui::Ui| {
            let _ = functora_egui::TabsValue::new(&entries).fill_width().show(
                ui,
                &mut selected,
                |_, _| {},
            );
        };
        let _ = app.step(&mut body);
        let out = app.step(&mut body);

        let boxes = box_rects(&out);
        assert_eq!(
            boxes.len(),
            3,
            "three tabs must paint at {width}px, got {boxes:?}"
        );
        let rows: Vec<f32> = boxes.iter().map(|b| b.min.y).collect();
        assert!(
            rows.iter().all(|y| (y - rows[0]).abs() < 2.0),
            "tabs that fit must share one row at {width}px, got {boxes:?}"
        );
    }
}

#[test]
fn label_text_stays_within_available_width() {
    let label_text =
        "A rather long label that has to be wrapped somewhere on narrow mobile screens".to_owned();
    let mut app = Harness::new(Vec2::new(360.0, 640.0));
    let mut column = Rect::NOTHING;
    let mut body = |ui: &mut egui::Ui| {
        let inner = ui.allocate_ui_with_layout(
            Vec2::new(200.0, 640.0),
            egui::Layout::top_down(egui::Align::Min),
            |col| {
                let _ = functora_egui::Label::new(label_text.clone()).show(col);
            },
        );
        column = inner.response.rect;
    };
    let out = app.step(&mut body);

    let (rect, text) = text_rects(&out)
        .into_iter()
        .next()
        .unwrap_or_else(|| panic!("label must render text"));
    assert_inside(rect, column, &format!("label {text:?}"));
    assert!(
        rect.width() <= 200.0,
        "label {text:?} is {:?} wide, wider than its 200px column",
        rect.width()
    );
}

#[test]
fn typography_wraps_a_long_word_inside_a_plain_row() {
    let url = "https://functora.github.io/apps/demo/?page=about&lang=en".to_owned();
    let mut app = Harness::new(Vec2::new(360.0, 640.0));
    let mut left = 0.0;
    let mut body = |ui: &mut egui::Ui| {
        let _ = ui.allocate_ui_with_layout(
            Vec2::new(200.0, 640.0),
            egui::Layout::top_down(egui::Align::Min),
            |col| {
                let _ = col.horizontal(|row| {
                    left = row.max_rect().min.x;
                    let _ = functora_egui::Typography::small(format!("Current polled: {url}"))
                        .show(row);
                });
            },
        );
    };
    let out = app.step(&mut body);

    let texts = text_rects(&out);
    assert!(!texts.is_empty(), "typography must render text");
    for (rect, text) in texts {
        assert!(
            rect.max.x <= left + 200.0 + 1.0,
            "typography {text:?} at {rect:?} runs past its 200px row starting at {left}"
        );
        assert!(
            rect.width() <= 200.0,
            "typography {text:?} is {:?} wide, wider than its 200px row",
            rect.width()
        );
    }
}

#[test]
fn a_button_stays_inside_a_container_narrower_than_itself() {
    let mut app = Harness::new(Vec2::new(360.0, 640.0));
    let mut column = Rect::NOTHING;
    let mut button = Rect::NOTHING;
    let mut body = |ui: &mut egui::Ui| {
        let _ = ui.allocate_ui_with_layout(
            Vec2::new(12.0, 640.0),
            egui::Layout::top_down(egui::Align::Min),
            |col| {
                column = col.max_rect();
                button = functora_egui::Button::new("Save").show(col).rect;
            },
        );
    };
    let out = app.step(&mut body);

    assert_ne!(button, Rect::NOTHING, "the button must lay out");
    assert_inside_x(button, column, "button box");
    for (rect, text) in text_rects(&out) {
        assert_inside_x(rect, button, &format!("button title {text:?}"));
    }
}

#[test]
fn fixed_width_button_contains_its_title() {
    let mut app = Harness::new(Vec2::new(360.0, 640.0));
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::Button::new("Сканировать заметку")
            .fixed_width(120.0)
            .show(ui);
    };
    let out = app.step(&mut body);

    let boxes = box_rects(&out);
    let host = boxes.first().copied().unwrap_or(Rect::NOTHING);
    assert_ne!(host, Rect::NOTHING, "button must paint its box");
    for (rect, text) in text_rects(&out) {
        assert_inside(rect, host, &format!("button title {text:?}"));
    }
}

#[test]
fn toolbar_groups_stay_inside_the_toolbar() {
    let screen = Vec2::new(320.0, 640.0);
    let mut app = Harness::new(screen);
    let icons = [
        functora_egui::LucideIcon::Copy,
        functora_egui::LucideIcon::Search,
        functora_egui::LucideIcon::Settings,
        functora_egui::LucideIcon::Trash,
        functora_egui::LucideIcon::Play,
        functora_egui::LucideIcon::MousePointer2,
        functora_egui::LucideIcon::Undo2,
        functora_egui::LucideIcon::Redo2,
    ];
    let mut frame = Rect::NOTHING;
    let mut body = |ui: &mut egui::Ui| {
        frame = functora_egui::Toolbar::new()
            .show(ui, |bar| {
                let _ = functora_egui::ButtonGroup::show(bar, |group| {
                    for icon in icons {
                        let _ = functora_egui::Button::icon_only(icon).show(group);
                    }
                });
                let _ = functora_egui::ButtonGroup::show(bar, |group| {
                    let _ = functora_egui::Button::icon_only(functora_egui::LucideIcon::Undo2)
                        .show(group);
                    let _ = functora_egui::Button::icon_only(functora_egui::LucideIcon::Redo2)
                        .show(group);
                });
                let _ = functora_egui::Button::new("Snap").show(bar);
            })
            .rect;
    };
    let out = app.step(&mut body);

    assert_ne!(frame, Rect::NOTHING, "toolbar must paint its frame");
    let bounds = Rect::from_min_size(Pos2::ZERO, screen);
    assert_inside(frame, bounds, "toolbar frame");
    for (idx, rect) in box_rects(&out).into_iter().enumerate() {
        assert_inside(rect, frame, &format!("toolbar box #{idx}"));
    }
    for (rect, text) in text_rects(&out) {
        assert_inside(rect, frame, &format!("toolbar label {text:?}"));
    }
}

#[test]
fn button_group_after_a_full_row_wraps_to_the_next_row() {
    let screen = Vec2::new(320.0, 640.0);
    let mut app = Harness::new(screen);
    let mut row = Rect::NOTHING;
    let mut grown = Rect::NOTHING;
    let mut body = |ui: &mut egui::Ui| {
        let _ = ui.horizontal_wrapped(|strip| {
            let full = strip.available_width();
            row = strip.max_rect();
            let _ = functora_egui::ButtonGroup::show(strip, |group| {
                let _ = functora_egui::Button::new("Fill")
                    .fixed_width(full)
                    .show(group);
            });
            let _ = functora_egui::ButtonGroup::show(strip, |group| {
                let _ =
                    functora_egui::Button::icon_only(functora_egui::LucideIcon::Copy).show(group);
                let _ =
                    functora_egui::Button::icon_only(functora_egui::LucideIcon::Search).show(group);
            });
            grown = strip.max_rect();
        });
    };
    let out = app.step(&mut body);

    assert_ne!(row, Rect::NOTHING, "the row must lay out");
    let boxes = box_rects(&out);
    assert_eq!(
        boxes.len(),
        3,
        "every button paints a visible box, got {boxes:?}"
    );
    for (idx, rect) in boxes.into_iter().enumerate() {
        assert_inside_x(rect, row, &format!("row box #{idx}"));
    }
    assert!(
        grown.max.x <= row.max.x,
        "the row grew past its container: {grown:?} vs {row:?}"
    );
}

#[test]
fn button_group_stays_on_screen() {
    let screen = Vec2::new(320.0, 640.0);
    let mut app = Harness::new(screen);
    let labels = [
        "Left alignment",
        "Center alignment",
        "Right alignment",
        "Justify content",
    ];
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::ButtonGroup::show(ui, |group| {
            for label in labels {
                let _ = functora_egui::Button::new(label)
                    .variant(functora_egui::ButtonVariant::Outline)
                    .show(group);
            }
        });
    };
    let out = app.step(&mut body);

    let bounds = Rect::from_min_size(Pos2::ZERO, screen);
    for (rect, text) in text_rects(&out) {
        assert_inside(rect, bounds, &format!("group label {text:?}"));
    }
    for (idx, rect) in box_rects(&out).into_iter().enumerate() {
        assert_inside(rect, bounds, &format!("group button box #{idx}"));
    }
}
