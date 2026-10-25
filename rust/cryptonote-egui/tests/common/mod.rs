#[cfg(not(target_arch = "wasm32"))]
use std::future::Future;

use cryptonote_egui::app::CryptonoteApp;

pub fn collect_shape(shape: &egui::Shape, rendered: &mut String) {
    match shape {
        egui::Shape::Text(text) => {
            rendered.push_str(text.galley.text());
            rendered.push('\n');
        }
        egui::Shape::Vec(shapes) => {
            for inner in shapes {
                collect_shape(inner, rendered);
            }
        }
        _ => {}
    }
}

pub fn collect_texts(shape: &egui::Shape, out: &mut Vec<(String, egui::Rect)>) {
    match shape {
        egui::Shape::Text(text) => out.push((
            text.galley.text().to_owned(),
            text.galley.rect.translate(text.pos.to_vec2()),
        )),
        egui::Shape::Vec(shapes) => {
            for inner in shapes {
                collect_texts(inner, out);
            }
        }
        _ => {}
    }
}

#[must_use]
pub fn all_texts(output: &egui::FullOutput) -> Vec<(String, egui::Rect)> {
    let mut found = Vec::new();
    for clipped in &output.shapes {
        collect_texts(&clipped.shape, &mut found);
    }
    found
}

pub fn run_app(app: &mut CryptonoteApp) -> String {
    use egui::{Pos2, RawInput, Rect, Vec2};
    let ctx = egui::Context::default();
    let raw = RawInput {
        screen_rect: Some(Rect::from_min_size(Pos2::ZERO, Vec2::new(1280.0, 800.0))),
        ..Default::default()
    };
    let mut frame = eframe::Frame::_new_kittest();
    let mut output = ctx.run_ui(raw, |ui| {
        eframe::App::ui(app, ui, &mut frame);
    });
    output.textures_delta.clear();
    let mut rendered = String::new();
    for clipped in &output.shapes {
        collect_shape(&clipped.shape, &mut rendered);
    }
    rendered
}

#[cfg(not(target_arch = "wasm32"))]
/// Drives a spawned worker task to completion.
///
/// # Panics
///
/// Panics if the task exits without producing a value.
#[must_use]
pub fn run_async<T, F>(future: F) -> T
where
    T: Send + 'static,
    F: Future<Output = T> + Send + 'static,
{
    match functora_egui::spawn_async(future).recv() {
        Ok(value) => value,
        Err(err) => panic!("worker task must complete: {err}"),
    }
}

const SCREEN: egui::Vec2 = egui::Vec2::new(1280.0, 800.0);

pub struct UiDriver {
    ctx: egui::Context,
    frame: u32,
}

impl UiDriver {
    #[must_use]
    pub fn new() -> Self {
        Self {
            ctx: egui::Context::default(),
            frame: 0,
        }
    }

    #[must_use]
    pub fn with_fonts() -> Self {
        let ctx = egui::Context::default();
        functora_egui::setup_fonts(&ctx);
        functora_egui::setup_image_loaders(&ctx);
        Self { ctx, frame: 0 }
    }

    fn input(&mut self, size: egui::Vec2, events: Vec<egui::Event>) -> egui::RawInput {
        self.frame += 1;
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(egui::Pos2::ZERO, size)),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        }
    }

    pub fn run(&mut self, app: &mut CryptonoteApp, events: Vec<egui::Event>) -> egui::FullOutput {
        self.run_sized(app, SCREEN, events)
    }

    pub fn run_sized(
        &mut self,
        app: &mut CryptonoteApp,
        size: egui::Vec2,
        events: Vec<egui::Event>,
    ) -> egui::FullOutput {
        let raw = self.input(size, events);
        let mut frame = eframe::Frame::_new_kittest();
        let mut out = self.ctx.run_ui(raw, |ui| {
            eframe::App::ui(app, ui, &mut frame);
        });
        out.textures_delta.clear();
        out
    }

    pub fn show(&mut self, events: Vec<egui::Event>, body: &mut dyn FnMut(&mut egui::Ui)) {
        let raw = self.input(SCREEN, events);
        let mut out = self.ctx.run_ui(raw, |ui| {
            let _ = egui::CentralPanel::default().show(ui, |inner_ui| body(inner_ui));
        });
        out.textures_delta.clear();
    }

    pub fn click(&mut self, app: &mut CryptonoteApp, pos: egui::Pos2) {
        self.click_sized(app, SCREEN, pos);
    }

    pub fn click_sized(&mut self, app: &mut CryptonoteApp, size: egui::Vec2, pos: egui::Pos2) {
        let press = |pressed: bool| egui::Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed,
            modifiers: egui::Modifiers::default(),
        };
        let _ = self.run_sized(app, size, vec![egui::Event::PointerMoved(pos)]);
        let _ = self.run_sized(app, size, vec![press(true)]);
        let _ = self.run_sized(app, size, vec![press(false)]);
        let _ = self.run_sized(app, size, vec![]);
    }
}

impl Default for UiDriver {
    fn default() -> Self {
        Self::new()
    }
}
