#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::route::Screen;
use cryptonote_egui::state::External;
use functora_egui::i18n::Language;

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
        egui::Shape::Text(text) => out.push((text.galley.text().to_owned(), text.galley.rect)),
        egui::Shape::Vec(shapes) => {
            for inner in shapes {
                collect_texts(inner, out);
            }
        }
        _ => {}
    }
}

fn find_label(output: &egui::FullOutput, label: &str) -> egui::Pos2 {
    let mut found = Vec::new();
    for clipped in &output.shapes {
        collect_texts(&clipped.shape, &mut found);
    }
    found
        .iter()
        .find(|(text, _)| text == label)
        .map(|(_, rect)| rect.center())
        .expect("reset label must render in the sidebar")
}

fn dirty_app() -> CryptonoteApp {
    let mut app = CryptonoteApp::default();
    "secret".clone_into(&mut app.temporary.note);
    "pw".clone_into(&mut app.temporary.password);
    app.temporary.cipher = None;
    "https://example.com/?screen=open&note=AAA".clone_into(&mut app.temporary.url_input);
    app.temporary.external = External::Note(cryptonote_egui::state::ExternalNote {
        data: cryptonote_egui::encoding::NoteData::PlainText("shared".to_owned()),
        url: "https://example.com/?screen=open&note=AAA".to_owned(),
    });
    app.temporary.screen = Screen::View;
    app.set_language(Language::Rus);
    app.toast.add("stale", functora_egui::ToastVariant::Error, 0.0);
    assert!(app.unseen_qr_error("boom"));
    cryptonote_egui::store_url("https://example.com/?screen=open&note=AAA".to_owned());
    cryptonote_egui::store_archive(cryptonote_egui::ArchiveSource::Bytes(vec![1, 2, 3]));
    app
}

#[test]
fn reset_restores_temporary_defaults() {
    let mut app = dirty_app();
    app.reset(0.0);
    assert_eq!(app.temporary.note, "");
    assert_eq!(app.temporary.password, "");
    assert_eq!(app.temporary.url_input, "");
    assert!(app.temporary.attachments.is_empty());
    assert!(matches!(app.temporary.external, External::Nothing));
    assert_eq!(app.temporary.screen, Screen::Home);
}

#[test]
fn sidebar_reset_button_clears_state() {
    use cryptonote_egui::i18n::I18N;
    let mut app = CryptonoteApp::default();
    app.temporary.note = "secret".to_owned();
    let mut driver = UiDriver::new();
    let out = driver.run(&mut app, vec![]);
    let label = cryptonote_egui::Msg::Reset.render(cryptonote_egui::detect_browser_language());
    let pos = find_label(&out, &label);
    driver.click(&mut app, pos);
    let _ = driver.run(&mut app, vec![]);
    assert_eq!(app.temporary.note, "");
    assert_eq!(app.temporary.screen, Screen::Home);
    assert_eq!(app.toast.next_id(), 1);
}

#[test]
fn reset_preserves_persistent_state_and_drains_intents() {
    use cryptonote_egui::i18n::I18N;
    let mut app = dirty_app();
    let persistent_before = app.persistent.clone();
    app.reset(1.0);
    assert_eq!(app.persistent, persistent_before);
    assert_eq!(app.persistent.language, Language::Rus);
    assert_eq!(app.toast.next_id(), 1);
    assert_eq!(
        cryptonote_egui::Msg::ResetDone.render(Language::Rus),
        "Приложение сброшено"
    );
    assert!(app.unseen_qr_error("boom"));
    assert!(cryptonote_egui::take_url().is_none());
    assert!(cryptonote_egui::take_archive().is_none());
}
