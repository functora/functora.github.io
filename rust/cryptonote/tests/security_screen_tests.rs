//! The Security screen renders the shipped security policy in every
//! supported language, the about text links to it through the word
//! "Security" alone, and the policy fits a narrow screen.

use cryptonote::app::CryptonoteApp;
use cryptonote::messages::Msg;
use cryptonote::route::Screen;
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;
use functora_egui::route::RouteMetadata;

pub mod common;
use common::{UiDriver, all_texts};

const SIDEBAR_X: f32 = 900.0;
const ABOUT_VIEWPORT: egui::Vec2 = egui::Vec2::new(1280.0, 3000.0);
const POLICY_VIEWPORT: egui::Vec2 = egui::Vec2::new(1280.0, 6000.0);
const PHONE_VIEWPORT: egui::Vec2 = egui::Vec2::new(390.0, 844.0);

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

fn rendered_text(output: &egui::FullOutput) -> String {
    all_texts(output)
        .iter()
        .map(|(text, _)| text.as_str())
        .collect::<Vec<_>>()
        .join("\n")
}

fn open_about(app: &mut CryptonoteApp, driver: &mut UiDriver, lang: Language) {
    let output = driver.run(app, vec![]);
    let position = sidebar_label(&output, &BaseMsg::About.render(lang));
    driver.click(app, position);
    let _ = driver.run(app, vec![]);
}

/// The sentence that carries the link to the Security screen, and the text
/// layout that renders it.
fn about_sentence(output: &egui::FullOutput, lang: Language) -> (egui::Pos2, std::sync::Arc<egui::Galley>) {
    let sentence = format!(
        "{}{}{}",
        Msg::AboutSecurityPrefix.render(lang),
        Msg::Security.render(lang),
        Msg::AboutSecuritySuffix.render(lang),
    );
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some((text.pos, std::sync::Arc::clone(&text.galley))),
            _ => None,
        })
        .find(|(_, galley)| galley.text() == sentence)
        .unwrap_or_else(|| panic!("the about sentence must render"))
}

/// The screen position of a word inside a rendered text layout.
fn word_position(pos: egui::Pos2, galley: &egui::Galley, word: &str) -> egui::Pos2 {
    let full = galley.text();
    let byte = full
        .find(word)
        .unwrap_or_else(|| panic!("{word:?} must render in the sentence"));
    let char_idx = full[..byte].chars().count();
    let glyph = galley.pos_from_cursor(egui::text::CCursor::new(char_idx));
    pos + glyph.center().to_vec2()
}

/// The color and the size of the text section that renders `needle`.
fn section_style(galley: &egui::Galley, needle: &str) -> (egui::Color32, f32) {
    let text = galley.job.text.as_str();
    let byte = text
        .find(needle)
        .unwrap_or_else(|| panic!("{needle:?} must render in the layout"));
    let index = egui::epaint::text::ByteIndex(byte);
    galley
        .job
        .sections
        .iter()
        .find(|section| section.byte_range.contains(&index))
        .map_or_else(
            || panic!("{needle:?} must have a text section"),
            |section| (section.format.color, section.format.font_id.size),
        )
}

/// The text layout that renders `needle`, if any.
fn layout_with(output: &egui::FullOutput, needle: &str) -> Option<std::sync::Arc<egui::Galley>> {
    output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some(std::sync::Arc::clone(&text.galley)),
            _ => None,
        })
        .find(|galley| galley.text().contains(needle))
}

fn open_security_screen(lang: Language) -> (CryptonoteApp, UiDriver) {
    let mut app = CryptonoteApp::default();
    app.set_language(lang);
    let mut driver = UiDriver::new();
    open_about(&mut app, &mut driver, lang);
    let output = driver.run_sized(&mut app, ABOUT_VIEWPORT, vec![]);
    let (pos, galley) = about_sentence(&output, lang);
    let target = word_position(pos, &galley, &Msg::Security.render(lang));
    driver.click_sized(&mut app, ABOUT_VIEWPORT, target);
    (app, driver)
}

#[test]
fn the_security_screen_renders_the_security_policy() {
    let (mut app, mut driver) = open_security_screen(Language::Eng);
    assert_eq!(app.temporary.screen, Screen::Security);

    let output = driver.run_sized(&mut app, POLICY_VIEWPORT, vec![]);
    let rendered = rendered_text(&output);

    for heading in [
        "Security Policy",
        "Reporting a vulnerability",
        "Trust boundaries",
        "Threat model",
    ] {
        assert!(
            rendered.contains(heading),
            "the Security screen must render {heading:?}"
        );
    }
}

#[test]
fn the_about_text_links_to_the_security_screen() {
    let (app, _) = open_security_screen(Language::Eng);
    assert_eq!(app.temporary.screen, Screen::Security);
    assert_eq!(Screen::Security.parent(), Some(Screen::Home));
    assert_eq!(Screen::Security.label(Language::Eng).as_ref(), "Security");
    assert_eq!(Screen::Security.label(Language::Spa).as_ref(), "Seguridad");
    assert_eq!(Screen::Security.label(Language::Rus).as_ref(), "Безопасность");
}

#[test]
fn the_about_text_links_to_the_security_screen_in_every_language() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let (app, _) = open_security_screen(lang);
        assert_eq!(
            app.temporary.screen,
            Screen::Security,
            "the about link in {lang:?} must open the Security screen"
        );
    }
}

#[test]
fn only_the_security_word_of_the_about_sentence_is_a_link() {
    let mut app = english_app();
    let mut driver = UiDriver::new();
    open_about(&mut app, &mut driver, Language::Eng);
    let output = driver.run_sized(&mut app, ABOUT_VIEWPORT, vec![]);
    let (pos, galley) = about_sentence(&output, Language::Eng);

    let prefix = Msg::AboutSecurityPrefix.render(Language::Eng);
    let plain_word = prefix
        .split_whitespace()
        .next()
        .unwrap_or_else(|| panic!("the prefix must have a word"));
    let plain = word_position(pos, &galley, plain_word);
    driver.click_sized(&mut app, ABOUT_VIEWPORT, plain);
    assert_eq!(
        app.temporary.screen,
        Screen::About,
        "the plain words of the sentence must not be a link"
    );

    let link = word_position(pos, &galley, &Msg::Security.render(Language::Eng));
    driver.click_sized(&mut app, ABOUT_VIEWPORT, link);
    assert_eq!(
        app.temporary.screen,
        Screen::Security,
        "the word Security must open the Security screen"
    );
}

#[test]
fn the_security_word_looks_like_the_other_links_of_the_screen() {
    let mut app = english_app();
    let mut driver = UiDriver::new();
    open_about(&mut app, &mut driver, Language::Eng);
    let output = driver.run_sized(&mut app, ABOUT_VIEWPORT, vec![]);
    let (_, galley) = about_sentence(&output, Language::Eng);

    let (link_color, link_size) = section_style(&galley, "Security");
    let (body_color, body_size) = section_style(&galley, "threat model");
    assert_eq!(link_size, body_size, "the link must use the size of the sentence");
    assert_ne!(link_color, body_color, "the link must not look like plain text");

    let beta = layout_with(&output, "Google Play").unwrap_or_else(|| panic!("the beta paragraph must render"));
    let (beta_color, _) = section_style(&beta, "Google Play");
    assert_eq!(
        link_color, beta_color,
        "the link must use the color of the other links of the screen"
    );
}

#[test]
fn the_security_screen_is_internationalized() {
    let expectations = [
        (
            Language::Eng,
            [
                "Security Policy",
                "Reporting a vulnerability",
                "Trust boundaries",
                "Threat model",
            ],
        ),
        (
            Language::Spa,
            [
                "Política de seguridad",
                "Cómo reportar una vulnerabilidad",
                "Límites de confianza",
                "Modelo de amenazas",
            ],
        ),
        (
            Language::Rus,
            [
                "Политика безопасности",
                "Как сообщить об уязвимости",
                "Границы доверия",
                "Модель угроз",
            ],
        ),
    ];

    for (lang, headings) in expectations {
        let (mut app, mut driver) = open_security_screen(lang);
        assert_eq!(app.temporary.screen, Screen::Security);
        let output = driver.run_sized(&mut app, POLICY_VIEWPORT, vec![]);
        let rendered = rendered_text(&output);
        for heading in headings {
            assert!(
                rendered.contains(heading),
                "the Security screen in {lang:?} must render {heading:?}"
            );
        }
    }
}

#[test]
fn the_security_screen_shows_the_accepted_dependency_warnings() {
    let expectations = [
        (Language::Eng, "Known accepted dependency warnings", "basic-toml"),
        (
            Language::Spa,
            "Dependencias con avisos conocidos y aceptados",
            "basic-toml",
        ),
        (
            Language::Rus,
            "Известные и принятые предупреждения о зависимостях",
            "basic-toml",
        ),
    ];

    for (lang, heading, warning) in expectations {
        let (mut app, mut driver) = open_security_screen(lang);
        let output = driver.run_sized(&mut app, POLICY_VIEWPORT, vec![]);
        let rendered = rendered_text(&output);
        assert!(
            rendered.contains(heading),
            "the Security screen in {lang:?} must render {heading:?}"
        );
        assert!(
            rendered.contains(warning),
            "the Security screen in {lang:?} must render the accepted warning about {warning:?}"
        );
    }
}

#[test]
fn the_security_screen_does_not_overflow_a_narrow_screen() {
    let (mut app, mut driver) = open_security_screen(Language::Eng);
    let output = driver.run_sized(&mut app, PHONE_VIEWPORT, vec![]);

    let overflowing: Vec<String> = output
        .shapes
        .iter()
        .filter_map(|clipped| match &clipped.shape {
            egui::Shape::Text(text) => Some((
                text.galley.rect.max.x + text.pos.x,
                text.galley.text().chars().take(40).collect::<String>(),
            )),
            _ => None,
        })
        .filter(|(right, _)| *right > PHONE_VIEWPORT.x)
        .map(|(_, text)| text)
        .collect();

    assert!(
        overflowing.is_empty(),
        "the Security screen must fit a {}px wide screen, but {overflowing:?} overflow it",
        PHONE_VIEWPORT.x
    );
}

#[test]
fn the_sidebar_has_no_separate_security_entry() {
    let mut app = english_app();
    let mut driver = UiDriver::new();
    let output = driver.run(&mut app, vec![]);
    let security = Msg::Security.render(Language::Eng);
    let rendered = all_texts(&output)
        .iter()
        .any(|(text, rect)| text == &security && rect.min.x > SIDEBAR_X);
    assert!(
        !rendered,
        "the Security screen is reached from the about text, not the sidebar"
    );
}

#[test]
fn the_plaintext_warning_names_who_can_read_the_note() {
    let expectations = [
        (Language::Eng, "Everyone who gets the link can read", "server"),
        (Language::Spa, "Cualquiera que reciba el enlace puede leer", "servidor"),
        (Language::Rus, "Любой, кто получит ссылку, сможет прочитать", "сервер"),
    ];

    for (lang, expected, forbidden) in expectations {
        let rendered = Msg::PlainShareWarning.render(lang);
        assert!(
            rendered.contains(expected),
            "the plaintext warning in {lang:?} must say who can read the note, got {rendered:?}"
        );
        assert!(
            !rendered.to_lowercase().contains(forbidden),
            "the plaintext warning in {lang:?} must not talk about a server, got {rendered:?}"
        );
    }
}

#[test]
fn the_about_text_does_not_talk_about_a_server() {
    let expectations = [
        (Language::Eng, "server"),
        (Language::Spa, "servidor"),
        (Language::Rus, "сервер"),
    ];

    for (lang, forbidden) in expectations {
        let rendered = Msg::AboutText.render(lang).to_lowercase();
        assert!(
            !rendered.contains(forbidden),
            "the about text in {lang:?} must not talk about a server"
        );
    }
}
