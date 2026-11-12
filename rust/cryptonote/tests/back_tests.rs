use cryptonote_egui::app::CryptonoteApp;
use cryptonote_egui::route::Screen;
use functora_egui::i18n::I18N;
use functora_egui::messages::Msg as BaseMsg;

pub mod common;
use common::{UiDriver, all_texts};

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
