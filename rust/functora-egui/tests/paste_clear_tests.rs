#![allow(clippy::unwrap_used, clippy::expect_used)]

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
}

fn click_at(app: &mut App, pos: Pos2, body: &mut dyn FnMut(&mut egui::Ui)) {
    let _ = app.step(
        vec![Event::PointerMoved(pos)],
        std::convert::identity::<&mut dyn FnMut(&mut egui::Ui)>(body),
    );
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
    let _ = app.step(vec![], body);
}

fn find_rects(output: &egui::FullOutput) -> Vec<Rect> {
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

#[test]
fn input_paste_clear_renders_without_panic() {
    let mut text = "hello".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::InputPasteClear::new(&mut text)
            .placeholder("placeholder")
            .show(ui);
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    assert!(!find_rects(&out).is_empty());
    assert_eq!(text, "hello");
}

#[test]
fn input_paste_clear_clear_resets_to_empty() {
    let mut text = "hello world".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::InputPasteClear::new(&mut text)
            .placeholder("type")
            .show(ui);
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let rects = find_rects(&out);
    let outer = rects
        .iter()
        .find(|r| (r.height() - 32.0).abs() < 2.0 && r.width() > 1000.0)
        .expect("outer rect not found");
    let pos = Pos2::new(outer.max.x - 10.0, outer.center().y);
    click_at(&mut app, pos, &mut body);
    assert_eq!(text, "");
}

#[test]
fn input_paste_clear_clear_resets_to_custom_default() {
    let mut text = "custom value".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::InputPasteClear::new(&mut text)
            .default_value("default123")
            .show(ui);
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let rects = find_rects(&out);
    let outer = rects
        .iter()
        .find(|r| (r.height() - 32.0).abs() < 2.0 && r.width() > 1000.0)
        .expect("outer rect not found");
    let pos = Pos2::new(outer.max.x - 10.0, outer.center().y);
    click_at(&mut app, pos, &mut body);
    assert_eq!(text, "default123");
}

#[test]
fn input_paste_clear_no_clear_when_already_default() {
    let mut text = String::new();
    let mut cleared = false;
    let mut body = |ui: &mut egui::Ui| {
        let resp = functora_egui::InputPasteClear::new(&mut text).show(ui);
        cleared = resp.cleared;
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let rects = find_rects(&out);
    let outer = rects
        .iter()
        .find(|r| (r.height() - 32.0).abs() < 2.0 && r.width() > 1000.0)
        .expect("outer rect not found");
    let pos = Pos2::new(outer.max.x - 10.0, outer.center().y);
    click_at(&mut app, pos, &mut body);
    assert!(!cleared);
    assert_eq!(text, "");
}

#[test]
fn input_paste_clear_custom_icons() {
    let mut text = "hello".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::InputPasteClear::new(&mut text)
            .paste_icon(functora_egui::LucideIcon::Copy)
            .clear_icon(functora_egui::LucideIcon::Trash)
            .show(ui);
    };
    let mut app = App::new();
    let out = app.step(vec![], &mut body);
    assert!(!find_rects(&out).is_empty());
    assert_eq!(text, "hello");
}

#[test]
fn input_paste_clear_password_does_not_panic() {
    let mut text = "secret123".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::InputPasteClear::new(&mut text)
            .password()
            .show(ui);
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    assert_eq!(text, "secret123");
}

#[test]
fn textarea_paste_clear_renders_without_panic() {
    let mut text = "multi\nline".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut text)
            .placeholder("placeholder")
            .min_height(80.0)
            .show(ui);
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    assert!(!find_rects(&out).is_empty());
}

#[test]
fn textarea_paste_clear_clear_works() {
    let mut text = "hello textarea".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut text)
            .placeholder("type")
            .show(ui);
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let rects = find_rects(&out);
    let outer = rects
        .iter()
        .find(|r| (r.height() - 64.0).abs() < 2.0 && r.width() > 1000.0)
        .expect("outer rect not found");
    let toolbar_y = outer.min.y + 14.0;
    let pos = Pos2::new(outer.max.x - 10.0, toolbar_y);
    click_at(&mut app, pos, &mut body);
    assert_eq!(text, "");
}

#[test]
fn textarea_paste_clear_custom_default() {
    let mut text = "value".to_owned();
    let mut body = |ui: &mut egui::Ui| {
        let _ = functora_egui::TextareaPasteClear::new(&mut text)
            .default_value("my default")
            .show(ui);
    };
    let mut app = App::new();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let rects = find_rects(&out);
    let outer = rects
        .iter()
        .find(|r| (r.height() - 64.0).abs() < 2.0 && r.width() > 1000.0)
        .expect("outer rect not found");
    let toolbar_y = outer.min.y + 14.0;
    let pos = Pos2::new(outer.max.x - 10.0, toolbar_y);
    click_at(&mut app, pos, &mut body);
    assert_eq!(text, "my default");
}
