use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::encoding::{NoteData, build_url};
use cryptonote_egui::route::Screen;
use functora_egui::i18n::Language;

pub mod common;
use common::run_app;

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
    let url = build_url("https://example.com/?screen=open", &note).unwrap_or_else(|err| panic!("build: {err}"));
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
