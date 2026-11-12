//! Invariant roundtrips: note encode/decode, URL query-param extraction,
//! screen string conversion and the cipher mapping must be bijections for
//! arbitrary inputs.
#![cfg(not(target_arch = "wasm32"))]

use cryptonote_egui::encoding::{NoteData, decode_note, encode_note};
use cryptonote_egui::route::Screen;
use cryptonote_egui::state::CipherChoice;
use proptest::prelude::*;
use proptest::test_runner::RngSeed;
use std::str::FromStr;

const SCREENS: [Screen; 9] = [
    Screen::Home,
    Screen::Open,
    Screen::View,
    Screen::Share,
    Screen::About,
    Screen::Donate,
    Screen::License,
    Screen::Privacy,
    Screen::File,
];

const CIPHERS: [CipherChoice; 3] = [
    CipherChoice::Plain,
    CipherChoice::Aes256Gcm,
    CipherChoice::ChaCha20Poly1305,
];

fn plain_text(note: &NoteData) -> Option<&str> {
    match note {
        NoteData::PlainText(text) => Some(text),
        NoteData::CipherText(_) => None,
    }
}

fn expect<T, E>(result: Result<T, E>, context: &str) -> T
where
    E: std::fmt::Display,
{
    match result {
        Ok(value) => value,
        Err(err) => panic!("{context}: {err}"),
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        rng_seed: RngSeed::Fixed(0x00C0_FFEE),
        ..ProptestConfig::default()
    })]

    #[test]
    fn plain_note_encode_decode_roundtrips(text in ".*") {
        let note = NoteData::PlainText(text.clone());
        let encoded = expect(encode_note(&note), "plain note must encode");
        let decoded = expect(decode_note(&encoded), "encoded note must decode");
        prop_assert_eq!(plain_text(&decoded), Some(text.as_str()));
    }

    #[test]
    fn note_url_roundtrips_through_query_param(
        base_url in "https?://[a-z0-9.-]+/?",
        text in ".*",
    ) {
        let note = NoteData::PlainText(text.clone());
        let url = expect(cryptonote_egui::encoding::build_url(&base_url, &note), "url must build");
        let extracted = expect(
            cryptonote_egui::encoding::extract_note_param(&url),
            "note param must exist",
        );
        let decoded = expect(decode_note(&extracted), "extracted note must decode");
        prop_assert_eq!(plain_text(&decoded), Some(text.as_str()));
    }

    #[test]
    fn extract_note_param_tolerates_unrelated_query_params(
        extra in "[a-z]{1,8}=[a-z]{1,8}",
        text in ".*",
    ) {
        let note = NoteData::PlainText(text.clone());
        let url =
            expect(cryptonote_egui::encoding::build_url("https://x.example/?", &note), "url must build");
        let with_extra = format!("{url}&{extra}");
        let extracted = expect(
            cryptonote_egui::encoding::extract_note_param(&with_extra),
            "note param must survive an extra param",
        );
        let decoded = expect(decode_note(&extracted), "extracted note must decode");
        prop_assert_eq!(plain_text(&decoded), Some(text.as_str()));
    }

    #[test]
    fn screen_display_from_str_roundtrips(index in 0..SCREENS.len()) {
        let screen = SCREENS[index];
        let rendered = screen.to_string();
        let parsed = expect(Screen::from_str(&rendered), "rendered screen must parse");
        prop_assert_eq!(parsed, screen);
    }

    #[test]
    fn cipher_choice_is_a_bijection(index in 0..CIPHERS.len()) {
        let choice = CIPHERS[index];
        let cipher: Option<_> = choice.into();
        let back = CipherChoice::from(cipher);
        prop_assert_eq!(back, choice);
    }
}
