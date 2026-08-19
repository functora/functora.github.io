#![allow(clippy::unwrap_used)]

use cryptonote_egui::Screen;
use std::str::FromStr;

#[test]
fn screen_display_and_from_str_roundtrip() {
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
        let parsed = Screen::from_str(&screen.to_string()).unwrap();
        assert_eq!(parsed, screen);
    }
}

#[test]
fn screen_from_str_rejects_unknown() {
    assert!(Screen::from_str("settings").is_err());
}

#[test]
fn theme_toggle_switches() {
    use cryptonote_egui::Theme;
    assert_eq!(Theme::Light.toggle(), Theme::Dark);
    assert_eq!(Theme::Dark.toggle(), Theme::Light);
}
