#![allow(clippy::unwrap_used, clippy::expect_used)]
use cryptonote_egui::crypto::{CipherType, decrypt_symmetric, encrypt_symmetric};
use cryptonote_egui::encoding::{NoteData, decode_note, encode_note};
use cryptonote_egui::route::Screen;
use cryptonote_egui::state::TemporaryState;
use functora_egui::i18n::Language;
use functora_egui::route::RouteMetadata;
use std::str::FromStr;

#[test]
fn screen_roundtrip() {
    for screen in [
        Screen::Home,
        Screen::Open,
        Screen::View,
        Screen::Share,
        Screen::About,
        Screen::Donate,
        Screen::License,
        Screen::Privacy,
        Screen::File,
    ] {
        let s = screen.to_string();
        let parsed = Screen::from_str(&s).expect("parse");
        assert_eq!(screen, parsed);
    }
}

#[test]
fn screen_default_is_home() {
    assert_eq!(Screen::default(), Screen::Home);
}

#[test]
fn route_metadata_parent() {
    assert_eq!(Screen::Home.parent(), None);
    assert_eq!(Screen::Open.parent(), Some(Screen::Home));
    assert_eq!(Screen::File.parent(), Some(Screen::View));
}

#[test]
fn route_metadata_children() {
    let children = Screen::Home.children();
    assert!(children.contains(&Screen::Open));
    assert!(children.contains(&Screen::View));
    assert!(children.contains(&Screen::Share));
    assert!(children.contains(&Screen::File));
    assert!(children.contains(&Screen::About));
}

#[test]
fn route_label() {
    assert_eq!(Screen::Home.label(Language::Eng).as_ref(), "Home");
    assert_eq!(Screen::Open.label(Language::Eng).as_ref(), "Open");
}

#[test]
fn temporary_state_default() {
    let state = TemporaryState::default();
    assert!(state.note.is_empty());
    assert!(state.password.is_empty());
    assert_eq!(state.cipher, Some(CipherType::Aes256Gcm));
    assert!(state.attachments.is_empty());
    assert_eq!(state.screen, Screen::Home);
}

#[test]
fn crypto_roundtrip_aes() {
    let plaintext = b"hello cryptonote";
    let password = "s3cret";
    let enc = encrypt_symmetric(plaintext, password, CipherType::Aes256Gcm).expect("encrypt");
    let dec = decrypt_symmetric(&enc, password).expect("decrypt");
    assert_eq!(plaintext.to_vec(), dec);
}

#[test]
fn crypto_roundtrip_chacha() {
    let plaintext = b"hello cryptonote chacha";
    let password = "another secret";
    let enc = encrypt_symmetric(plaintext, password, CipherType::ChaCha20Poly1305).expect("encrypt");
    let dec = decrypt_symmetric(&enc, password).expect("decrypt");
    assert_eq!(plaintext.to_vec(), dec);
}

#[test]
fn encoding_plain_roundtrip() {
    let note = NoteData::PlainText("hello world".to_string());
    let encoded = encode_note(&note).expect("encode");
    let decoded = decode_note(&encoded).expect("decode");
    match decoded {
        NoteData::PlainText(t) => assert_eq!(t, "hello world"),
        NoteData::CipherText(_) => panic!("expected plaintext"),
    }
}

#[test]
fn encoding_cipher_roundtrip() {
    let enc = encrypt_symmetric(b"secret note", "pwd", CipherType::Aes256Gcm).expect("encrypt");
    let note = NoteData::CipherText(enc);
    let encoded = encode_note(&note).expect("encode");
    let decoded = decode_note(&encoded).expect("decode");
    match decoded {
        NoteData::CipherText(c) => {
            let dec = decrypt_symmetric(&c, "pwd").expect("decrypt");
            assert_eq!(dec, b"secret note");
        }
        NoteData::PlainText(_) => panic!("expected ciphertext"),
    }
}

#[test]
fn build_url_contains_note() {
    let note = NoteData::PlainText("test".to_string());
    let url = cryptonote_egui::encoding::build_url("https://example.com/?screen=open", &note).expect("build");
    assert!(url.contains("note="));
}

#[test]
fn extract_note_param_roundtrip() {
    let note = NoteData::PlainText("extract test".to_string());
    let url = cryptonote_egui::encoding::build_url("https://example.com", &note).expect("build");
    let param = cryptonote_egui::encoding::extract_note_param(&url).expect("extract");
    let decoded = decode_note(&param).expect("decode");
    match decoded {
        NoteData::PlainText(t) => assert_eq!(t, "extract test"),
        NoteData::CipherText(_) => panic!("expected plaintext"),
    }
}
