use cryptonote_egui::encoding::{NoteData, build_url, decode_note, extract_note_param};
use cryptonote_egui::error::AppError;
use cryptonote_egui::hooks::handle_open_url;
use cryptonote_egui::route::Screen;
use cryptonote_egui::state::TemporaryState;

#[test]
fn shared_url_with_screen_open_roundtrips() {
    let note = NoteData::PlainText("hello shared".to_string());
    let url = build_url("https://example.com/?screen=open", &note).unwrap_or_else(|err| panic!("build: {err}"));
    assert!(url.contains("screen=open"));
    assert!(url.contains("note="));
    let param = extract_note_param(&url).unwrap_or_else(|err| panic!("extract: {err}"));
    let decoded = decode_note(&param).unwrap_or_else(|err| panic!("decode: {err}"));
    match decoded {
        NoteData::PlainText(text) => assert_eq!(text, "hello shared"),
        NoteData::CipherText(_) => panic!("expected plaintext"),
    }
}

#[test]
fn extract_note_param_strips_trailing_fragment() {
    let note = NoteData::PlainText("fragment case".to_string());
    let url = build_url("https://example.com/?screen=open", &note).unwrap_or_else(|err| panic!("build: {err}"));
    let with_fragment = format!("{url}#section");
    let param = extract_note_param(&with_fragment).unwrap_or_else(|err| panic!("extract: {err}"));
    let decoded = decode_note(&param).unwrap_or_else(|err| panic!("decode must succeed despite #fragment: {err}"));
    match decoded {
        NoteData::PlainText(text) => assert_eq!(text, "fragment case"),
        NoteData::CipherText(_) => panic!("expected plaintext"),
    }
}

#[test]
fn handle_open_url_plaintext_goes_to_view() {
    let note = NoteData::PlainText("plain via url".to_string());
    let url = build_url("https://example.com/?screen=open", &note).unwrap_or_else(|err| panic!("build: {err}"));
    let mut state = TemporaryState::default();
    let screen = handle_open_url(&url, &mut state).unwrap_or_else(|err| panic!("open: {err}"));
    assert_eq!(screen, Screen::View);
    assert_eq!(state.note, "plain via url");
}

#[test]
fn handle_open_url_missing_param_is_no_note_param() {
    let mut state = TemporaryState::default();
    let err = handle_open_url("https://example.com/?screen=open", &mut state)
        .err()
        .unwrap_or_else(|| panic!("open url without note param must fail"));
    assert!(matches!(err, AppError::NoNoteParam), "unexpected: {err:?}");
}

#[test]
fn handle_open_url_empty_input_is_no_note_param() {
    let mut state = TemporaryState::default();
    let err = handle_open_url("", &mut state)
        .err()
        .unwrap_or_else(|| panic!("open url with empty input must fail"));
    assert!(matches!(err, AppError::NoNoteParam), "unexpected: {err:?}");
}
