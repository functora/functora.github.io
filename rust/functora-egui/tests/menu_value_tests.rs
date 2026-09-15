//! Typed menu/command widgets bind values directly instead of blind
//! `usize` indexes: entries pair each value with its label.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};
use std::cell::Cell;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    Cut,
    Copy,
    Paste,
}

fn entries() -> [(Action, String); 3] {
    [
        (Action::Cut, "Cut entry".to_owned()),
        (Action::Copy, "Copy entry".to_owned()),
        (Action::Paste, "Paste entry".to_owned()),
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

    fn text_center(out: &egui::FullOutput, label: &str) -> Pos2 {
        out.shapes
            .iter()
            .find_map(|clipped| match &clipped.shape {
                Shape::Text(text) if text.galley.text() == label => {
                    Some(text.pos + text.galley.size() / 2.0)
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("rendered text {label:?} not found"))
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
    let _ = app.step(vec![], body);
}

#[test]
fn dropdown_menu_value_click_returns_picked_value() {
    let all = entries();
    let picked: Rc<Cell<Option<Action>>> = Rc::new(Cell::new(None));
    let trigger_rect: Rc<Cell<Option<Rect>>> = Rc::new(Cell::new(None));
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let trigger = ui.add(functora_egui::Button::new("Open menu"));
        trigger_rect.set(Some(trigger.rect));
        functora_egui::DropdownMenu::show_value(ui, &trigger, &all, |value| {
            picked.set(Some(value));
        });
    };
    let _ = app.step(vec![], &mut body);
    let center = trigger_rect
        .get()
        .expect("trigger rect must be recorded")
        .center();
    click_at(&mut app, center, &mut body);
    let open = app.step(vec![], &mut body);
    assert!(
        Harness::texts(&open)
            .iter()
            .any(|t| t.contains("Copy entry")),
        "open menu must list entry labels"
    );
    let target = Harness::text_center(&open, "Copy entry");
    click_at(&mut app, target, &mut body);
    assert_eq!(picked.get(), Some(Action::Copy));
}

#[test]
fn dropdown_menu_value_reports_nothing_without_choice() {
    let all = entries();
    let fired = Cell::new(false);
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let trigger = ui.add(functora_egui::Button::new("Open menu"));
        functora_egui::DropdownMenu::show_value(ui, &trigger, &all, |_: Action| {
            fired.set(true);
        });
    };
    let _ = app.step(vec![], &mut body);
    let _ = app.step(vec![], &mut body);
    assert!(!fired.get(), "unopened menu must not report a choice");
}

#[test]
fn context_menu_value_click_returns_picked_value() {
    let all = entries();
    let picked: Rc<Cell<Option<Action>>> = Rc::new(Cell::new(None));
    let target_rect: Rc<Cell<Option<Rect>>> = Rc::new(Cell::new(None));
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let response = ui.add(functora_egui::Button::new("Right-click target"));
        target_rect.set(Some(response.rect));
        functora_egui::ContextMenu::show_value(&response, &all, |value| {
            picked.set(Some(value));
        });
    };
    let _ = app.step(vec![], &mut body);
    let center = target_rect
        .get()
        .expect("target rect must be recorded")
        .center();
    let _ = app.step(vec![Event::PointerMoved(center)], &mut body);
    let _ = app.step(
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Secondary,
            pressed: true,
            modifiers: egui::Modifiers::default(),
        }],
        &mut body,
    );
    let _ = app.step(
        vec![Event::PointerButton {
            pos: center,
            button: egui::PointerButton::Secondary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
        &mut body,
    );
    let open = app.step(vec![], &mut body);
    assert!(
        Harness::texts(&open)
            .iter()
            .any(|t| t.contains("Paste entry")),
        "open context menu must list entry labels"
    );
    let target = Harness::text_center(&open, "Paste entry");
    click_at(&mut app, target, &mut body);
    assert_eq!(picked.get(), Some(Action::Paste));
}

#[test]
fn menubar_menu_value_click_returns_picked_value() {
    let all = entries();
    let picked: Rc<Cell<Option<Action>>> = Rc::new(Cell::new(None));
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::Menubar::new().show(ui, |bar| {
            functora_egui::Menubar::menu_value(bar, "Edit", &all, |value| {
                picked.set(Some(value));
            });
        });
    };
    let before = app.step(vec![], &mut body);
    let trigger = Harness::text_center(&before, "Edit");
    click_at(&mut app, trigger, &mut body);
    let open = app.step(vec![], &mut body);
    assert!(
        Harness::texts(&open)
            .iter()
            .any(|t| t.contains("Cut entry")),
        "open submenu must list entry labels"
    );
    let target = Harness::text_center(&open, "Cut entry");
    click_at(&mut app, target, &mut body);
    assert_eq!(picked.get(), Some(Action::Cut));
}

#[test]
fn command_value_click_returns_picked_value_and_closes() {
    use functora_egui::{CommandItem, CommandValue, LucideIcon};
    let all = [
        (
            Action::Cut,
            CommandItem {
                group: "Edit".to_owned(),
                group_icon: LucideIcon::Pencil,
                label: "Cut command".to_owned(),
                icon: LucideIcon::Scissors,
            },
        ),
        (
            Action::Copy,
            CommandItem {
                group: "Edit".to_owned(),
                group_icon: LucideIcon::Pencil,
                label: "Copy command".to_owned(),
                icon: LucideIcon::Copy,
            },
        ),
    ];
    let mut open = true;
    let mut search = String::new();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let picked = CommandValue::new(all.to_vec()).show(ui.ctx(), &mut open, &mut search);
        assert!(picked.is_none(), "nothing picked before any click");
    };
    let _ = app.step(vec![], &mut body);
    let shown = app.step(vec![], &mut body);
    assert!(
        Harness::texts(&shown)
            .iter()
            .any(|t| t.contains("Copy command")),
        "open palette must list command labels"
    );
    let target = Harness::text_center(&shown, "Copy command");
    let mut picked = None;
    let mut click_body = |ui: &mut egui::Ui| {
        if let Some(value) = CommandValue::new(all.to_vec()).show(ui.ctx(), &mut open, &mut search)
        {
            picked = Some(value);
        }
    };
    click_at(&mut app, target, &mut click_body);
    assert_eq!(picked, Some(Action::Copy));
    assert!(!open, "palette must close after picking");
    assert!(
        search.is_empty(),
        "palette must clear the search after picking"
    );
}

#[test]
fn command_value_search_filters_entries() {
    use functora_egui::{CommandItem, CommandValue, LucideIcon};
    let all = [
        (
            Action::Cut,
            CommandItem {
                group: "Edit".to_owned(),
                group_icon: LucideIcon::Pencil,
                label: "Cut command".to_owned(),
                icon: LucideIcon::Scissors,
            },
        ),
        (
            Action::Paste,
            CommandItem {
                group: "Edit".to_owned(),
                group_icon: LucideIcon::Pencil,
                label: "Paste command".to_owned(),
                icon: LucideIcon::ClipboardPaste,
            },
        ),
    ];
    let mut open = true;
    let mut search = "paste".to_owned();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = CommandValue::new(all.to_vec()).show(ui.ctx(), &mut open, &mut search);
    };
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let texts = Harness::texts(&out);
    assert!(
        texts.iter().any(|t| t.contains("Paste command")),
        "filter must keep the matching entry"
    );
    assert!(
        !texts.iter().any(|t| t.contains("Cut command")),
        "filter must hide the non-matching entry"
    );
}
