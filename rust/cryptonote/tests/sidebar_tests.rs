//! The shared default sidebar drives the cryptonote navigation, the reset and
//! the desktop-only quit entry.

use cryptonote::app::CryptonoteApp;
use cryptonote::route::Screen;
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::route::RouteMetadata;
use functora_egui::white_label::{QUIT_AVAILABLE, default_sidebar_labels};

pub mod common;
use common::{UiDriver, all_texts};

const SIDEBAR_X: f32 = 900.0;

fn english_app() -> CryptonoteApp {
    let mut app = CryptonoteApp::default();
    app.set_language(Language::Eng);
    app
}

fn sidebar_label(output: &egui::FullOutput, label: &str) -> egui::Pos2 {
    all_texts(output)
        .iter()
        .find(|(text, rect)| text == label && rect.min.x > SIDEBAR_X)
        .map_or_else(
            || panic!("{label:?} must render in the sidebar"),
            |(_, rect)| rect.center(),
        )
}

fn closes_window(output: &egui::FullOutput) -> bool {
    output
        .viewport_output
        .get(&egui::ViewportId::ROOT)
        .is_some_and(|viewport| viewport.commands.contains(&egui::ViewportCommand::Close))
}

#[test]
fn sidebar_labels_match_the_shared_defaults() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let labels = default_sidebar_labels(lang);
        assert_eq!(labels[0], Screen::Home.label(lang));
        assert_eq!(labels[1], Screen::About.label(lang));
        assert_eq!(labels[2], Screen::Donate.label(lang));
    }
}

#[test]
fn sidebar_quit_entry_follows_platform_availability() {
    let mut app = english_app();
    let mut driver = UiDriver::new();
    let output = driver.run(&mut app, vec![]);
    let quit = BaseMsg::Quit.render(Language::Eng);
    let rendered = all_texts(&output)
        .iter()
        .any(|(text, rect)| text == &quit && rect.min.x > SIDEBAR_X);
    assert_eq!(
        rendered, QUIT_AVAILABLE,
        "the quit entry must be rendered exactly where it is available"
    );
}

#[test]
fn clicking_quit_closes_the_window_without_navigating() {
    let mut app = english_app();
    let mut driver = UiDriver::new();
    let output = driver.run(&mut app, vec![]);
    let quit = BaseMsg::Quit.render(Language::Eng);
    let position = sidebar_label(&output, &quit);
    let press = |pressed: bool| egui::Event::PointerButton {
        pos: position,
        button: egui::PointerButton::Primary,
        pressed,
        modifiers: egui::Modifiers::default(),
    };
    let outputs = [
        driver.run(&mut app, vec![egui::Event::PointerMoved(position)]),
        driver.run(&mut app, vec![press(true)]),
        driver.run(&mut app, vec![press(false)]),
        driver.run(&mut app, vec![]),
    ];
    assert!(
        outputs.iter().any(closes_window),
        "clicking quit must ask the window to close"
    );
    assert_eq!(app.temporary.screen, Screen::Home);
}

#[test]
fn sidebar_entries_navigate() {
    let mut app = english_app();
    let mut driver = UiDriver::new();
    let output = driver.run(&mut app, vec![]);
    let about = BaseMsg::About.render(Language::Eng);
    let position = sidebar_label(&output, &about);
    driver.click(&mut app, position);
    let _ = driver.run(&mut app, vec![]);
    assert_eq!(app.temporary.screen, Screen::About);
}
