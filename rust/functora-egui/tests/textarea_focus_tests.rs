#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Empty textarea must gain focus when clicking anywhere inside its border,
//! not only on the first text line.

use egui::{Context, Event, Pos2, RawInput, Rect, Shape, Vec2};

const SCREEN: Vec2 = Vec2::new(1280.0, 800.0);

struct App {
    ctx: Context,
    frame: u32,
}

impl App {
    fn fresh() -> Self {
        let ctx = Context::default();
        functora_egui::setup_fonts(&ctx);
        Self { ctx, frame: 0 }
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
    let _ = app.step(vec![], body);
}

fn border_boxes(output: &egui::FullOutput) -> Vec<Rect> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            Shape::Rect(rect) if rect.stroke.width > 0.0 => Some(rect.rect),
            _ => None,
        })
        .collect()
}

#[test]
fn empty_textarea_bottom_click_focuses() {
    let mut text = String::new();
    let focused = std::cell::Cell::new(false);
    let mut body = |ui: &mut egui::Ui| {
        let resp = functora_egui::Textarea::new(&mut text)
            .min_height(192.0)
            .show(ui);
        focused.set(resp.has_focus());
    };
    let mut app = App::fresh();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let outer = border_boxes(&out)
        .into_iter()
        .max_by(|a, b| {
            a.height()
                .partial_cmp(&b.height())
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .expect("textarea border must render");
    let pos = Pos2::new(outer.center().x, outer.max.y - 12.0);
    assert!(
        outer.contains(pos),
        "bottom click {pos:?} must be inside {outer:?}"
    );
    click_at(&mut app, pos, &mut body);
    assert!(
        focused.get(),
        "clicking empty textarea bottom {pos:?} inside {outer:?} must focus"
    );
}

#[test]
#[cfg(feature = "clipboard")]
fn empty_textarea_paste_clear_bottom_click_focuses() {
    let mut text = String::new();
    let focused = std::cell::Cell::new(false);
    let mut body = |ui: &mut egui::Ui| {
        let resp = functora_egui::TextareaPasteClear::new(&mut text)
            .min_height(192.0)
            .show(ui);
        focused.set(resp.response.has_focus());
    };
    let mut app = App::fresh();
    let _ = app.step(vec![], &mut body);
    let out = app.step(vec![], &mut body);
    let outer = border_boxes(&out)
        .into_iter()
        .max_by(|a, b| {
            a.height()
                .partial_cmp(&b.height())
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .expect("textarea border must render");
    let pos = Pos2::new(outer.center().x, outer.max.y - 12.0);
    assert!(
        outer.contains(pos),
        "bottom click {pos:?} must be inside {outer:?}"
    );
    click_at(&mut app, pos, &mut body);
    assert!(
        focused.get(),
        "clicking empty TextareaPasteClear bottom {pos:?} inside {outer:?} must focus"
    );
}
