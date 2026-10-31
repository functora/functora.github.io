use functora_core::i18n::{I18N, Language};
use functora_core::{AppAttrs, Msg, WhiteLabelContent, donate_blocks};

const ATTRS: AppAttrs = AppAttrs {
    app: "cryptonote-egui",
    vsn: "0.1.10",
    org: "functora",
    src: Some("rust"),
    dst: "apps",
    description: "Cryptonote is a cross-platform, serverless app for encrypted offline notes.",
};

#[test]
fn versioned_urls_carry_app_version() {
    assert_eq!(
        ATTRS.app_version_url(),
        "https://functora.github.io/apps/cryptonote-egui/0.1.10"
    );
    assert_eq!(
        ATTRS.origin(),
        "https://functora.github.io/apps/cryptonote-egui/0.1.10"
    );
}

#[test]
fn default_content_uses_functora_defaults() {
    let content = WhiteLabelContent::<Msg>::default();
    assert_eq!(content.license_text, None);
    assert_eq!(content.privacy_text, None);
    assert_eq!(content.donate_greeting, None);
    assert_eq!(content.donate_intro, None);
    assert_eq!(content.donate_blocks, donate_blocks());
}

#[test]
fn content_overrides_are_preserved() {
    let content = WhiteLabelContent {
        license_text: Some(Msg::Copyright),
        privacy_text: None,
        donate_greeting: Some(Msg::Donate),
        donate_intro: None,
        donate_blocks: vec![],
    };
    assert_eq!(content.license_text, Some(Msg::Copyright));
    assert_eq!(content.donate_greeting, Some(Msg::Donate));
    assert!(
        content.donate_blocks.is_empty(),
        "donate_blocks must be empty: {:?}",
        content.donate_blocks
    );
}

#[test]
fn merged_messages_render_in_all_languages() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert_ne!(Msg::Copyright.render(lang), "");
        assert_ne!(Msg::AllRightsReserved.render(lang), "");
        assert_ne!(Msg::ByContinuing.render(lang), "");
        assert_ne!(Msg::YouAgree.render(lang), "");
        assert_ne!(Msg::TermsOfService.render(lang), "");
        assert_ne!(Msg::TermsOfServiceTitle.render(lang), "");
        assert_ne!(Msg::PrivacyPolicyAnd.render(lang), "");
        assert_ne!(Msg::PrivacyPolicyTitle.render(lang), "");
        assert_ne!(Msg::VersionLabel.render(lang), "");
        assert_ne!(Msg::Application.render(lang), "");
        assert_ne!(Msg::Theme.render(lang), "");
        assert_ne!(Msg::Donate.render(lang), "");
        assert_ne!(Msg::DonateLink.render(lang), "");
        assert_ne!(Msg::And.render(lang), "");
        assert_ne!(Msg::FooterShareWord.render(lang), "");
        assert_ne!(Msg::FooterAppWord.render(lang), "");
        assert!(Msg::LicenseText.render(lang).contains("Copyright"));
        assert_ne!(Msg::PrivacyText.render(lang), "");
    }
    assert!(
        Msg::PrivacyText
            .render(Language::Eng)
            .contains("Privacy Policy")
    );
}

#[test]
fn language_flags_and_names_render() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert_ne!(Msg::LanguageFlag(lang).render(Language::Eng), "🌐");
        assert_ne!(Msg::LanguageName(lang).render(Language::Eng), "");
    }
    assert_eq!(Msg::LanguageFlag(Language::Fra).render(Language::Eng), "🌐");
    assert_eq!(
        Msg::LanguageName(Language::Fra).render(Language::Eng),
        "Unknown"
    );
}
