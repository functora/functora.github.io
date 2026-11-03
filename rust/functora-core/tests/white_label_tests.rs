use functora_core::i18n::{I18N, Language};
use functora_core::{
    AppAttrs, FUNCTORA_EMAIL, FUNCTORA_ORG, Msg, WhiteLabelContent, default_about_text,
    default_donate_blocks, default_donate_greeting, default_donate_intro, default_license_text,
    default_privacy_text, donate_blocks, functora_attrs,
};

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

#[test]
fn functora_attrs_uses_functora_preset() {
    let attrs = functora_attrs("cryptonote-egui", "0.1.10", "A cross-platform app.");
    assert_eq!(attrs.app, "cryptonote-egui");
    assert_eq!(attrs.vsn, "0.1.10");
    assert_eq!(attrs.org, FUNCTORA_ORG);
    assert_eq!(attrs.src, Some("rust"));
    assert_eq!(attrs.dst, "apps");
    assert_eq!(attrs.description, "A cross-platform app.");
    assert_eq!(attrs.app_name(), "Cryptonote-egui");
}

#[test]
fn default_license_text_is_mit_and_i18n() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let text = default_license_text(&ATTRS, lang);
        assert!(!text.is_empty(), "license text empty for {lang:?}");
        assert!(text.contains("MIT"), "missing MIT header for {lang:?}");
        assert!(text.contains("Copyright"), "missing copyright for {lang:?}");
        assert!(text.contains(FUNCTORA_ORG), "missing org for {lang:?}");
    }
    assert!(default_license_text(&ATTRS, Language::Eng).contains("AS IS"));
}

#[test]
fn default_privacy_text_derives_from_attrs() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let text = default_privacy_text(&ATTRS, lang);
        assert!(!text.is_empty(), "privacy text empty for {lang:?}");
        assert!(
            text.contains("Privacy Policy")
                || text.contains("Política de Privacidad")
                || text.contains("Политика конфиденциальности"),
            "missing title for {lang:?}"
        );
        assert!(text.contains(FUNCTORA_ORG), "missing provider for {lang:?}");
        assert!(
            text.contains(FUNCTORA_EMAIL),
            "missing contact for {lang:?}"
        );
    }
}

#[test]
fn default_donate_text_derives_from_attrs() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let greeting = default_donate_greeting(&ATTRS, lang);
        let intro = default_donate_intro(&ATTRS, lang);
        assert!(!greeting.is_empty(), "donate greeting empty for {lang:?}");
        assert!(!intro.is_empty(), "donate intro empty for {lang:?}");
        assert!(
            greeting.contains(FUNCTORA_ORG),
            "missing org in greeting for {lang:?}"
        );
        assert!(
            intro.contains(FUNCTORA_ORG),
            "missing org in intro for {lang:?}"
        );
    }
    assert!(default_donate_greeting(&ATTRS, Language::Eng).starts_with("Hello"));
}

#[test]
fn default_about_text_contains_links() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        let text = default_about_text(&ATTRS, lang);
        assert!(!text.is_empty(), "about text empty for {lang:?}");
        assert!(
            text.contains(ATTRS.app_name().as_str()),
            "missing app name for {lang:?}"
        );
        assert!(
            text.contains(ATTRS.description),
            "missing description for {lang:?}"
        );
        assert!(
            text.contains(ATTRS.source_url().as_str()),
            "missing source link for {lang:?}"
        );
        assert!(
            text.contains(ATTRS.author_url().as_str()),
            "missing author link for {lang:?}"
        );
        assert!(
            text.contains(ATTRS.beta_url().as_str()),
            "missing beta link for {lang:?}"
        );
        assert!(
            text.contains(ATTRS.google_play_url().as_str()),
            "missing play link for {lang:?}"
        );
        assert!(
            text.contains(ATTRS.apk_url().as_str()),
            "missing apk link for {lang:?}"
        );
    }
}

#[test]
fn default_donate_blocks_contain_btc_and_xmr() {
    let blocks = default_donate_blocks();
    assert_eq!(blocks.len(), 2);
    assert!(
        blocks.iter().any(|b| b.label.contains("BTC")
            && b.address == "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q")
    );
    assert!(
        blocks
            .iter()
            .any(|b| b.label.contains("XMR") && b.address.starts_with("48sTw"))
    );
}
