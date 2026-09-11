//! Behavioral contracts for overlay/data widgets without dedicated tests
//! (plan step 6a): dialogs render only when open, toasts appear and expire,
//! calendars clamp invalid months, comboboxes show placeholders.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use egui::{Context, Pos2, RawInput, Rect, Shape, Vec2};

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

    fn step(&mut self, body: &mut dyn FnMut(&mut egui::Ui)) -> egui::FullOutput {
        self.frame += 1;
        let raw = RawInput {
            screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
            time: Some(f64::from(self.frame) / 60.0),
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
}

#[test]
fn dialog_closed_renders_nothing_and_stays_closed() {
    let mut open = false;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        functora_egui::Dialog::new()
            .title("Edit Profile")
            .description("Make changes.")
            .show(ui.ctx(), &mut open, |_| {});
    };
    let out = app.step(&mut body);
    assert!(!open);
    assert!(
        !Harness::texts(&out)
            .iter()
            .any(|t| t.contains("Edit Profile")),
        "closed dialog must not render its title"
    );
}

#[test]
fn dialog_open_renders_title_and_description() {
    let mut open = true;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        functora_egui::Dialog::new()
            .title("Edit Profile")
            .description("Make changes.")
            .show(ui.ctx(), &mut open, |_| {});
    };
    let _ = app.step(&mut body);
    let out = app.step(&mut body);
    let texts = Harness::texts(&out);
    assert!(texts.iter().any(|t| t.contains("Edit Profile")));
    assert!(texts.iter().any(|t| t.contains("Make changes.")));
}

#[test]
fn alert_dialog_closed_returns_open_and_renders_nothing() {
    use functora_egui::AlertDialogResult;
    use std::cell::Cell;
    let mut open = false;
    let saw_open = Cell::new(false);
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let result =
            functora_egui::AlertDialog::new("Are you sure?", "No undo.").show(ui.ctx(), &mut open);
        saw_open.set(matches!(result, AlertDialogResult::Open));
    };
    let out = app.step(&mut body);
    assert!(!open);
    assert!(saw_open.get(), "closed alert must report Open");
    assert!(
        !Harness::texts(&out)
            .iter()
            .any(|t| t.contains("Are you sure?")),
        "closed alert must not render"
    );
}

#[test]
fn alert_dialog_open_renders_title() {
    let mut open = true;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::AlertDialog::new("Are you sure?", "No undo.")
            .destructive()
            .show(ui.ctx(), &mut open);
    };
    let _ = app.step(&mut body);
    let out = app.step(&mut body);
    assert!(
        Harness::texts(&out)
            .iter()
            .any(|t| t.contains("Are you sure?"))
    );
}

#[test]
fn sheet_closed_renders_nothing_and_stays_closed() {
    let mut open = false;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        functora_egui::Sheet::new()
            .title("Sheet Panel")
            .show(ui.ctx(), &mut open, |_| {});
    };
    let out = app.step(&mut body);
    assert!(!open);
    assert!(
        !Harness::texts(&out)
            .iter()
            .any(|t| t.contains("Sheet Panel")),
        "closed sheet must not render its title"
    );
}

#[test]
fn sheet_open_renders_title() {
    let mut open = true;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        functora_egui::Sheet::new()
            .title("Sheet Panel")
            .show(ui.ctx(), &mut open, |_| {});
    };
    let _ = app.step(&mut body);
    let out = app.step(&mut body);
    assert!(
        Harness::texts(&out)
            .iter()
            .any(|t| t.contains("Sheet Panel"))
    );
}

#[test]
fn toast_appears_then_expires() {
    let mut toasts = functora_egui::ToastState::new();
    toasts.add("Hello toast", functora_egui::ToastVariant::Default, 0.0);
    let mut app = Harness::new();
    let out = {
        // Toast areas settle on the second frame, like other overlays.
        let mut warmup = |ui: &mut egui::Ui| toasts.show(ui.ctx());
        let _ = app.step(&mut warmup);
        let mut body = |ui: &mut egui::Ui| toasts.show(ui.ctx());
        app.step(&mut body)
    };
    assert!(
        Harness::texts(&out)
            .iter()
            .any(|t| t.contains("Hello toast"))
    );
    toasts.cleanup(60.0);
    let out_after = {
        let mut body_after = |ui: &mut egui::Ui| toasts.show(ui.ctx());
        app.step(&mut body_after)
    };
    assert!(
        !Harness::texts(&out_after)
            .iter()
            .any(|t| t.contains("Hello toast")),
        "expired toast must disappear"
    );
}

#[test]
fn calendar_clamps_invalid_month_and_renders() {
    let mut year = 2026;
    let mut month = 13;
    let mut day = 1;
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::Calendar::new().show(ui, &mut year, &mut month, &mut day);
    };
    let out = app.step(&mut body);
    assert_eq!(month, 12, "month must clamp to December");
    assert!(
        !Harness::texts(&out).is_empty(),
        "calendar must render cells"
    );
}

#[test]
fn combobox_shows_placeholder_without_selection() {
    let items = vec!["React".to_owned(), "Vue".to_owned()];
    let mut selected: Option<usize> = None;
    let mut search = String::new();
    let mut app = Harness::new();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::Combobox::new(items.clone())
            .placeholder("Select framework...")
            .show(ui, &mut selected, &mut search);
    };
    let out = app.step(&mut body);
    assert_eq!(selected, None);
    assert!(
        Harness::texts(&out)
            .iter()
            .any(|t| t.contains("Select framework...")),
        "placeholder must render without a selection"
    );
}
