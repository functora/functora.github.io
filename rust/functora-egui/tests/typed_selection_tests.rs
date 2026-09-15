//! Typed selection widgets bind enum values directly instead of blind
//! `usize` indexes: entries pair each value with its label.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
use std::cell::Cell;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Fruit {
    Apple,
    Banana,
    Cherry,
}

fn entries() -> [(Fruit, String); 3] {
    [
        (Fruit::Apple, "Apple".to_owned()),
        (Fruit::Banana, "Banana".to_owned()),
        (Fruit::Cherry, "Cherry".to_owned()),
    ]
}

struct Harness {
    ctx: Context,
    frame: u32,
}

impl Harness {
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
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner| body(inner));
        });
        out.textures_delta.clear();
        out
    }

    fn texts(out: &egui::FullOutput) -> Vec<String> {
        out.shapes
            .iter()
            .filter_map(|clipped| match &clipped.shape {
                Shape::Text(text) => Some(text.galley.text().to_owned()),
                _ => None,
            })
            .collect()
    }

    fn filled_rects(out: &egui::FullOutput) -> Vec<Rect> {
        out.shapes
            .iter()
            .filter_map(|clipped| match &clipped.shape {
                Shape::Rect(rect_shape) => Some(rect_shape.rect),
                _ => None,
            })
            .collect()
    }
}

fn click_at(app: &mut Harness, pos: Pos2, body: &mut dyn FnMut(&mut egui::Ui)) {
    let _ = app.step(vec![Event::PointerMoved(pos)], body);
    let _ = app.step(
        vec![Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed: true,
            modifiers: egui::Modifiers::default(),
        }],
        body,
    );
    let _ = app.step(
        vec![Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
        body,
    );
}

/// Tab/item rects are painted inside their container frame: keep rects that
/// are strictly contained in another modestly-sized rect, ordered left to right.
fn inner_tab_rects(out: &egui::FullOutput) -> Vec<Rect> {
    let modest: Vec<Rect> = Harness::filled_rects(out)
        .into_iter()
        .filter(|r| r.height() < 200.0 && r.width() < 600.0)
        .collect();
    let mut inner: Vec<Rect> = modest
        .iter()
        .filter(|candidate| {
            modest.iter().any(|outer| {
                outer != *candidate
                    && outer.contains_rect(**candidate)
                    && outer.area() > candidate.area()
            })
        })
        .copied()
        .collect();
    inner.sort_by(|a, b| {
        a.min
            .x
            .partial_cmp(&b.min.x)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    inner.dedup();
    inner
}

#[test]
fn tabs_value_renders_labels_and_reports_selection() {
    let all = entries();
    let mut selected = Fruit::Banana;
    let mut seen = None;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TabsValue::new(&all).show(ui, &mut selected, |_, value| {
            seen = Some(*value);
        });
    };
    let out = app.step(vec![], &mut body);
    assert_eq!(selected, Fruit::Banana);
    assert_eq!(seen, Some(Fruit::Banana));
    let texts = Harness::texts(&out);
    for label in ["Apple", "Banana", "Cherry"] {
        assert!(
            texts.iter().any(|t| t.contains(label)),
            "tab bar must render {label}"
        );
    }
}

#[test]
fn tabs_value_click_selects_second_entry() {
    let all = entries();
    let mut selected = Fruit::Apple;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TabsValue::new(&all).show(ui, &mut selected, |_, _| {});
    };
    let out = app.step(vec![], &mut body);
    let tabs = inner_tab_rects(&out);
    assert_eq!(tabs.len(), 3, "expected three tab rects, got {tabs:?}");
    click_at(&mut app, tabs[1].center(), &mut body);
    assert_eq!(selected, Fruit::Banana);
}

#[test]
fn tabs_value_unknown_value_keeps_selection() {
    let known = [(Fruit::Apple, "Apple".to_owned())];
    let mut selected = Fruit::Cherry;
    let mut seen = None;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TabsValue::new(&known).show(ui, &mut selected, |_, value| {
            seen = Some(*value);
        });
    };
    let _ = app.step(vec![], &mut body);
    assert_eq!(selected, Fruit::Cherry);
    assert_eq!(seen, Some(Fruit::Cherry));
}

#[test]
fn icon_tabs_value_click_selects_second_entry() {
    use functora_egui::{IconTabsValue, TabEntry};
    let all = [
        (Fruit::Apple, TabEntry::Text("Apple".to_owned())),
        (Fruit::Banana, TabEntry::Text("Banana".to_owned())),
        (Fruit::Cherry, TabEntry::Text("Cherry".to_owned())),
    ];
    let mut selected = Fruit::Apple;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = IconTabsValue::new(&all).show(ui, &mut selected, |_, _| {});
    };
    let out = app.step(vec![], &mut body);
    let tabs = inner_tab_rects(&out);
    assert_eq!(tabs.len(), 3, "expected three tab rects, got {tabs:?}");
    click_at(&mut app, tabs[2].center(), &mut body);
    assert_eq!(selected, Fruit::Cherry);
}

#[test]
fn toggle_group_value_click_selects_second_entry() {
    let all = entries();
    let mut selected = Fruit::Apple;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::ToggleGroupValue::new(&all).show(ui, &mut selected);
    };
    let out = app.step(vec![], &mut body);
    let texts = Harness::texts(&out);
    for label in ["Apple", "Banana", "Cherry"] {
        assert!(
            texts.iter().any(|t| t.contains(label)),
            "toggle group must render {label}"
        );
    }
    let items = inner_tab_rects(&out);
    assert_eq!(items.len(), 3, "expected three item rects, got {items:?}");
    click_at(&mut app, items[1].center(), &mut body);
    assert_eq!(selected, Fruit::Banana);
}

#[test]
fn toggle_group_value_unknown_value_keeps_selection() {
    let known = [(Fruit::Apple, "Apple".to_owned())];
    let mut selected = Fruit::Cherry;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::ToggleGroupValue::new(&known).show(ui, &mut selected);
    };
    let _ = app.step(vec![], &mut body);
    assert_eq!(selected, Fruit::Cherry);
}

#[test]
fn navigation_menu_value_reports_click_and_keeps_quiet_without() {
    let all = entries();
    let mut active = Fruit::Apple;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::NavigationMenuValue::new(&all).show(ui, &mut active);
    };
    let out = app.step(vec![], &mut body);
    assert_eq!(active, Fruit::Apple);
    let texts = Harness::texts(&out);
    for label in ["Apple", "Banana", "Cherry"] {
        assert!(
            texts.iter().any(|t| t.contains(label)),
            "navigation menu must render {label}"
        );
    }
    let painted: Vec<Rect> = Harness::filled_rects(&out)
        .into_iter()
        .filter(|r| r.height() < 200.0 && r.width() < 600.0 && r.top() < 200.0)
        .collect();
    assert_eq!(
        painted.len(),
        1,
        "only the active item paints, got {painted:?}"
    );
    let mut clicked = None;
    let mut click_body = |ui: &mut egui::Ui| {
        clicked = functora_egui::NavigationMenuValue::new(&all).show(ui, &mut active);
    };
    click_at(&mut app, painted[0].center(), &mut click_body);
    assert_eq!(clicked, Some(Fruit::Apple));
    assert_eq!(active, Fruit::Apple);
}

#[test]
fn combobox_value_renders_selected_label_or_placeholder() {
    let all = entries();
    let mut app = Harness::new();
    let mut selected = Some(Fruit::Banana);
    let mut search = String::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::ComboboxValue::new(&all).show(ui, &mut selected, &mut search);
    };
    let out = app.step(vec![], &mut body);
    assert!(
        Harness::texts(&out).iter().any(|t| t.contains("Banana")),
        "trigger must show the selected entry label"
    );
    let mut missing = Some(Fruit::Cherry);
    let known = [(Fruit::Apple, "Apple".to_owned())];
    let mut missing_body = |ui: &mut egui::Ui| {
        let _ = functora_egui::ComboboxValue::new(&known)
            .placeholder("Pick one...")
            .show(ui, &mut missing, &mut search);
    };
    let missing_out = app.step(vec![], &mut missing_body);
    assert!(
        Harness::texts(&missing_out)
            .iter()
            .any(|t| t.contains("Pick one...")),
        "unknown value must fall back to the placeholder"
    );
}

#[test]
fn combobox_value_popup_lists_all_entries() {
    let all = entries();
    let mut selected = Some(Fruit::Apple);
    let mut search = String::new();
    let trigger_rect: Rc<Cell<Option<Rect>>> = Rc::new(Cell::new(None));
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let response = functora_egui::ComboboxValue::new(&all).show(ui, &mut selected, &mut search);
        trigger_rect.set(Some(response.rect));
    };
    let before = app.step(vec![], &mut body);
    assert!(
        !Harness::texts(&before).iter().any(|t| t.contains("Banana")),
        "closed popup must not leak entry labels"
    );
    let center = trigger_rect
        .get()
        .expect("trigger rect must be recorded")
        .center();
    click_at(&mut app, center, &mut body);
    let after = app.step(vec![], &mut body);
    let texts = Harness::texts(&after);
    for label in ["Apple", "Banana", "Cherry"] {
        assert!(
            texts.iter().any(|t| t.contains(label)),
            "open popup must list {label}"
        );
    }
}

#[test]
fn select_labeled_renders_selected_label_or_placeholder() {
    let all = entries();
    let mut app = Harness::new();
    let mut selected = Some(Fruit::Cherry);
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::SelectLabeled::new(&mut selected, &all).show(ui);
    };
    let out = app.step(vec![], &mut body);
    assert!(
        Harness::texts(&out).iter().any(|t| t.contains("Cherry")),
        "trigger must show the selected entry label"
    );
    let mut missing: Option<Fruit> = None;
    let mut missing_body = |ui: &mut egui::Ui| {
        let _ = functora_egui::SelectLabeled::new(&mut missing, &all)
            .placeholder("Pick one...")
            .show(ui);
    };
    let missing_out = app.step(vec![], &mut missing_body);
    assert!(
        Harness::texts(&missing_out)
            .iter()
            .any(|t| t.contains("Pick one...")),
        "empty selection must show the placeholder"
    );
}

#[test]
fn select_labeled_popup_lists_all_entries() {
    let all = entries();
    let mut selected = Some(Fruit::Apple);
    let trigger_rect: Rc<Cell<Option<Rect>>> = Rc::new(Cell::new(None));
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let response = functora_egui::SelectLabeled::new(&mut selected, &all).show(ui);
        trigger_rect.set(Some(response.rect));
    };
    let _ = app.step(vec![], &mut body);
    let center = trigger_rect
        .get()
        .expect("trigger rect must be recorded")
        .center();
    click_at(&mut app, center, &mut body);
    let after = app.step(vec![], &mut body);
    let texts = Harness::texts(&after);
    for label in ["Apple", "Banana", "Cherry"] {
        assert!(
            texts.iter().any(|t| t.contains(label)),
            "open popup must list {label}"
        );
    }
}

#[test]
fn select_value_labeled_renders_selected_label() {
    let all = entries();
    let mut selected = Fruit::Banana;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::SelectValueLabeled::new(&mut selected, &all).show(ui);
    };
    let out = app.step(vec![], &mut body);
    assert_eq!(selected, Fruit::Banana);
    assert!(
        Harness::texts(&out).iter().any(|t| t.contains("Banana")),
        "trigger must show the selected entry label"
    );
}

#[test]
fn radio_group_labeled_renders_labels_and_keeps_selection() {
    let all = entries();
    let mut selected = Fruit::Cherry;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::RadioGroupLabeled::new(&mut selected, &all).show(ui);
    };
    let out = app.step(vec![], &mut body);
    assert_eq!(selected, Fruit::Cherry);
    let texts = Harness::texts(&out);
    for label in ["Apple", "Banana", "Cherry"] {
        assert!(
            texts.iter().any(|t| t.contains(label)),
            "radio group must render {label}"
        );
    }
}
