use functora_dioxus::i18n::{I18N, Language};
use functora_dioxus::{Msg, WhiteLabelContent, donate_blocks};

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
    assert!(content.donate_blocks.is_empty());
}

#[test]
fn merged_messages_render_in_all_languages() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert!(!Msg::Copyright.render(lang).is_empty());
        assert!(!Msg::AllRightsReserved.render(lang).is_empty());
        assert!(!Msg::ByContinuing.render(lang).is_empty());
        assert!(!Msg::YouAgree.render(lang).is_empty());
        assert!(!Msg::TermsOfService.render(lang).is_empty());
        assert!(!Msg::TermsOfServiceTitle.render(lang).is_empty());
        assert!(!Msg::PrivacyPolicyAnd.render(lang).is_empty());
        assert!(!Msg::PrivacyPolicyTitle.render(lang).is_empty());
        assert!(!Msg::VersionLabel.render(lang).is_empty());
        assert!(!Msg::Application.render(lang).is_empty());
        assert!(!Msg::Theme.render(lang).is_empty());
        assert!(!Msg::Donate.render(lang).is_empty());
        assert!(!Msg::DonateLink.render(lang).is_empty());
        assert!(!Msg::And.render(lang).is_empty());
        assert!(!Msg::FooterShareWord.render(lang).is_empty());
        assert!(!Msg::FooterAppWord.render(lang).is_empty());
        assert!(Msg::LicenseText.render(lang).contains("Copyright"));
        assert!(!Msg::PrivacyText.render(lang).is_empty());
    }
    assert!(Msg::PrivacyText.render(Language::Eng).contains("Privacy Policy"));
}

#[test]
fn language_flags_and_names_render() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert_ne!(Msg::LanguageFlag(lang).render(Language::Eng), "🌐");
        assert!(!Msg::LanguageName(lang).render(Language::Eng).is_empty());
    }
    assert_eq!(Msg::LanguageFlag(Language::Fra).render(Language::Eng), "🌐");
    assert_eq!(Msg::LanguageName(Language::Fra).render(Language::Eng), "Unknown");
}
