#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::route::Screen;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;

struct UiDriver {
    ctx: egui::Context,
    frame: u32,
}

impl UiDriver {
    fn new() -> Self {
        Self {
            ctx: egui::Context::default(),
            frame: 0,
        }
    }

    fn run(&mut self, app: &mut CryptonoteApp, events: Vec<egui::Event>) -> egui::FullOutput {
        self.frame += 1;
        let raw = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::ZERO,
                egui::Vec2::new(1280.0, 800.0),
            )),
            time: Some(f64::from(self.frame) / 60.0),
            events,
            ..Default::default()
        };
        let mut frame = eframe::Frame::_new_kittest();
        let mut out = self.ctx.run_ui(raw, |ui| {
            eframe::App::ui(app, ui, &mut frame);
        });
        out.textures_delta.clear();
        out
    }

    fn click(&mut self, app: &mut CryptonoteApp, pos: egui::Pos2) {
        let press = |pressed: bool| egui::Event::PointerButton {
            pos,
            button: egui::PointerButton::Primary,
            pressed,
            modifiers: egui::Modifiers::default(),
        };
        let _ = self.run(app, vec![egui::Event::PointerMoved(pos)]);
        let _ = self.run(app, vec![press(true)]);
        let _ = self.run(app, vec![press(false)]);
        let _ = self.run(app, vec![]);
    }
}

fn collect_texts(shape: &egui::Shape, out: &mut Vec<(String, egui::Rect)>) {
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

fn all_texts(output: &egui::FullOutput) -> Vec<(String, egui::Rect)> {
    let mut found = Vec::new();
    for clipped in &output.shapes {
        collect_texts(&clipped.shape, &mut found);
    }
    found
}

fn lang() -> functora_egui::i18n::Language {
    cryptonote_egui::detect_browser_language()
}

fn click_label(driver: &mut UiDriver, app: &mut CryptonoteApp, label: &str) {
    let out = driver.run(app, vec![]);
    let pos = all_texts(&out)
        .iter()
        .find(|(text, _)| text == label)
        .map_or_else(|| panic!("label {label:?} must render"), |(_, rect)| rect.center());
    driver.click(app, pos);
    let _ = driver.run(app, vec![]);
}

#[test]
fn dock_back_returns_to_previous_screen() {
    let mut app = CryptonoteApp::default();
    let mut driver = UiDriver::new();
    click_label(&mut driver, &mut app, &cryptonote_egui::Msg::ViewButton.render(lang()));
    assert_eq!(app.temporary.screen, Screen::View);
    click_label(&mut driver, &mut app, &BaseMsg::Back.render(lang()));
    assert_eq!(app.temporary.screen, Screen::Home);
}

#[test]
fn dock_back_hidden_without_history() {
    let mut app = CryptonoteApp::default();
    let mut driver = UiDriver::new();
    let out = driver.run(&mut app, vec![]);
    let back = BaseMsg::Back.render(lang());
    assert!(
        all_texts(&out).iter().all(|(text, _)| text != &back),
        "Back must not render when going back is impossible"
    );
}
