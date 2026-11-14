//! The password strength and plaintext advisories are warnings, not
//! blockers: the share still happens. Only an empty password blocks, because
//! the ciphertext would then be trivially decryptable by anyone.

use cryptonote::crypto::CipherType;
use cryptonote::hooks::{is_weak_password, share_error, share_warning};
use cryptonote::messages::Msg;
use functora_egui::i18n::{I18N, Language};

const STRONG: &str = "correct horse battery staple";

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
