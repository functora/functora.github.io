//! The password strength and plaintext advisories are warnings, not
//! blockers: the share still happens. Only an empty password blocks, because
//! the ciphertext would then be trivially decryptable by anyone. The view
//! screen warns before it puts the note on the clipboard or into an
//! unencrypted archive, because both leak it to the rest of the device.

use cryptonote::app::CryptonoteApp;
use cryptonote::crypto::CipherType;
use cryptonote::encoding::{NoteData, build_url};
use cryptonote::hooks::{is_weak_password, share_error, share_warning};
use cryptonote::messages::Msg;
use cryptonote::route::Screen;
use cryptonote::state::ActionMode;
use functora_egui::i18n::{I18N, Language};
use functora_egui::messages::Msg as BaseMsg;

pub mod common;
use common::{UiDriver, all_texts};

const STRONG: &str = "correct horse battery staple";
const BASE_URL: &str = "https://functora.github.io/apps/cryptonote/0.2.0/?screen=open";
const NOTE: &str = "the note body";

#[test]
fn a_weak_password_warns_but_does_not_block() {
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        assert!(
            share_error(Some(cipher), "short").is_none(),
            "a weak password must not block the share"
        );
        assert!(
            matches!(share_warning(Some(cipher), "short"), Some(Msg::WeakPasswordWarning)),
            "a weak password must warn"
        );
    }
}

#[test]
fn a_strong_password_does_not_warn() {
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        assert!(share_error(Some(cipher), STRONG).is_none());
        assert!(share_warning(Some(cipher), STRONG).is_none());
    }
}

#[test]
fn an_empty_password_still_blocks() {
    for cipher in [CipherType::Aes256Gcm, CipherType::ChaCha20Poly1305] {
        assert!(
            share_error(Some(cipher), "").is_some(),
            "an empty password must block the share"
        );
    }
}

#[test]
fn plain_mode_warns_but_does_not_block() {
    assert!(
        share_error(None, "").is_none(),
        "plain mode needs no password and must not block"
    );
    assert!(
        matches!(share_warning(None, ""), Some(Msg::PlainShareWarning)),
        "plain mode must warn about the cleartext link"
    );
    assert!(matches!(share_warning(None, STRONG), Some(Msg::PlainShareWarning)));
}

#[test]
fn the_weakness_threshold_is_documented() {
    assert_eq!(cryptonote::hooks::WEAK_PASSWORD_MIN_LEN, 12);
    assert!(is_weak_password(&"a".repeat(11)));
    assert!(!is_weak_password(&"a".repeat(12)));
    assert!(!is_weak_password(STRONG));
}

#[test]
fn every_warning_renders_in_every_language() {
    for msg in [
        Msg::WeakPasswordWarning,
        Msg::PlainShareWarning,
        Msg::ClipboardPlaintextWarning,
        Msg::UnencryptedExportWarning,
    ] {
        for lang in [Language::Eng, Language::Spa, Language::Rus] {
            assert!(
                !msg.render(lang).is_empty(),
                "empty warning translation for {msg:?} in {lang:?}"
            );
        }
    }
}

/// The position of a rendered label, so a test can click it.
fn label_position(output: &egui::FullOutput, label: &str) -> egui::Pos2 {
    all_texts(output)
        .iter()
        .find(|(text, _)| text == label)
        .map_or_else(|| panic!("{label:?} must render"), |(_, rect)| rect.center())
}

/// The rendered text of the frame, with the line breaks of the layout
/// flattened, so a wrapped warning still matches.
fn rendered_text(output: &egui::FullOutput) -> String {
    all_texts(output)
        .iter()
        .map(|(text, _)| text.as_str())
        .collect::<Vec<_>>()
        .join(" ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn view_app() -> CryptonoteApp {
    let mut app = CryptonoteApp::default();
    app.set_language(Language::Eng);
    app.temporary.action = ActionMode::Open;
    app.temporary.url_input =
        build_url(BASE_URL, &NoteData::PlainText(NOTE.to_owned())).unwrap_or_else(|e| panic!("build_url: {e:?}"));
    app.temporary.attachments = vec![functora_egui::files::Attachment {
        name: "passport.pdf".to_owned(),
        data: std::sync::Arc::from(b"the attachment bytes".to_vec()),
    }];
    app
}

/// Open the note of the app and land on the view screen.
fn open_note(app: &mut CryptonoteApp, driver: &mut UiDriver) {
    let output = driver.run(app, vec![]);
    driver.click(app, label_position(&output, &Msg::OpenButton.render(Language::Eng)));
    let _ = driver.run(app, vec![]);
    assert_eq!(app.temporary.screen, Screen::View);
}

#[test]
fn the_view_screen_warns_before_it_copies_to_the_clipboard() {
    let mut app = view_app();
    let mut driver = UiDriver::new();
    open_note(&mut app, &mut driver);

    let before = driver.run(&mut app, vec![]);
    driver.click(&mut app, label_position(&before, &BaseMsg::Copy.render(Language::Eng)));
    let after = driver.run(&mut app, vec![]);

    assert!(
        rendered_text(&after).contains(&Msg::ClipboardPlaintextWarning.render(Language::Eng)),
        "the clipboard copy must warn, got {:?}",
        rendered_text(&after)
    );
}

#[test]
fn the_view_screen_warns_before_it_exports_the_plain_attachments() {
    let mut app = view_app();
    let mut driver = UiDriver::new();
    open_note(&mut app, &mut driver);

    let before = driver.run(&mut app, vec![]);
    driver.click(&mut app, label_position(&before, &Msg::SaveAll.render(Language::Eng)));
    let after = driver.run(&mut app, vec![]);

    assert!(
        rendered_text(&after).contains(&Msg::UnencryptedExportWarning.render(Language::Eng)),
        "the unencrypted export must warn, got {:?}",
        rendered_text(&after)
    );
}
