#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::encoding::{NoteData, build_url};
use cryptonote_egui::route::Screen;
use functora_egui::i18n::Language;

fn collect_shape(shape: &egui::Shape, rendered: &mut String) {
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

fn run_app(app: &mut CryptonoteApp) -> String {
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

fn other_language() -> Language {
    match cryptonote_egui::detect_browser_language() {
        Language::Rus => Language::Spa,
        _ => Language::Rus,
    }
}

fn no_note_text(lang: Language) -> &'static str {
    match lang {
        Language::Rus => "Заметка не найдена в URL",
        _ => "No se encontró nota en la URL",
    }
}

#[test]
fn empty_view_banner_follows_selected_language() {
    let mut app = CryptonoteApp::default();
    let lang = other_language();
    app.set_language(lang);
    let note = NoteData::PlainText("x".to_string());
    let url = build_url("https://example.com/?screen=open", &note).expect("build");
    cryptonote_egui::store_url(url);
    let _ = run_app(&mut app);
    assert_eq!(app.temporary.screen, Screen::View);
    app.temporary.note.clear();
    let rendered = run_app(&mut app);
    assert!(
        rendered.contains(no_note_text(lang)),
        "empty View must show the no-note error in the selected language, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("No note found in URL"),
        "empty View must not leak the English error text, got:\n{rendered}"
    );
}

#[test]
fn switching_language_clears_stale_toasts() {
    let mut app = CryptonoteApp::default();
    app.toast.add("stale", functora_egui::ToastVariant::Error, 0.0);
    assert_eq!(app.toast.next_id(), 1);
    app.set_language(cryptonote_egui::detect_browser_language());
    assert_eq!(
        app.toast.next_id(),
        1,
        "re-selecting the active language must keep toasts"
    );
    app.set_language(other_language());
    assert_eq!(
        app.toast.next_id(),
        0,
        "switching language must drop toasts rendered in the old language"
    );
}

#[test]
fn switching_language_resets_qr_error_dedup() {
    let mut app = CryptonoteApp::default();
    assert!(app.unseen_qr_error("boom"));
    assert!(!app.unseen_qr_error("boom"));
    app.set_language(cryptonote_egui::detect_browser_language());
    assert!(
        !app.unseen_qr_error("boom"),
        "re-selecting the active language must keep qr dedup"
    );
    app.set_language(other_language());
    assert!(
        app.unseen_qr_error("boom"),
        "switching language must allow re-notifying the qr error in the new language"
    );
}
