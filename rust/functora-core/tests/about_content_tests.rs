use functora_core::i18n::{I18N, Language};
use functora_core::{AppAttrs, FUNCTORA_CORE_DATE, FUNCTORA_CORE_YEAR, Msg};

const ATTRS: AppAttrs = AppAttrs {
    app: "cryptonote",
    vsn: "0.1.10",
    org: "functora",
    src: Some("rust"),
    dst: "apps",
    description: "Encrypted offline notes with file attachments.",
};

#[test]
fn source_url_derives_repo_root() {
    assert_eq!(
        AppAttrs { src: None, ..ATTRS }.source_url(),
        "https://github.com/functora/functora.github.io"
    );
}

#[test]
fn source_url_derives_tree_path() {
    assert_eq!(
        ATTRS.source_url(),
        "https://github.com/functora/functora.github.io/tree/master/rust/cryptonote"
    );
}

#[test]
fn pages_url_derives_org_site_root() {
    assert_eq!(ATTRS.pages_url(), "https://functora.github.io");
}

#[test]
fn app_url_derives_pages_app_subpath() {
    assert_eq!(
        ATTRS.app_url(),
        "https://functora.github.io/apps/cryptonote"
    );
}

#[test]
fn app_name_capitalizes_first_letter() {
    assert_eq!(ATTRS.app_name(), "Cryptonote");
    assert_eq!(
        AppAttrs {
            app: "Cryptonote",
            ..ATTRS
        }
        .app_name(),
        "Cryptonote"
    );
    assert_eq!(
        AppAttrs {
            app: "delivery-calculator",
            ..ATTRS
        }
        .app_name(),
        "Delivery-calculator"
    );
}

#[test]
fn apk_url_derives_release_tag() {
    assert_eq!(
        ATTRS.apk_url(),
        "https://github.com/functora/functora.github.io/releases/tag/cryptonote-v0.1.10"
    );
}

#[test]
fn google_play_url_derives_org_package() {
    assert_eq!(
        ATTRS.google_play_url(),
        "https://play.google.com/store/apps/details?id=com.functora.cryptonote"
    );
}

#[test]
fn beta_url_derives_org_group() {
    assert_eq!(ATTRS.beta_url(), "https://groups.google.com/g/functora");
}

#[test]
fn author_url_derives_org_site() {
    assert_eq!(ATTRS.author_url(), "https://functora.github.io/");
}

#[test]
fn share_anchor_id_is_stable_and_app_specific() {
    assert_eq!(ATTRS.share_anchor_id(), ATTRS.share_anchor_id());
    assert_ne!(
        ATTRS.share_anchor_id(),
        AppAttrs {
            app: "delivery-calculator",
            ..ATTRS
        }
        .share_anchor_id()
    );
}

#[test]
fn about_messages_render_all_languages() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert!(!Msg::CopyAppLink.render(lang).is_empty());
        assert!(!Msg::ShareAppLink.render(lang).is_empty());
        assert!(!Msg::Sent.render(lang).is_empty());
        assert!(!Msg::SourceCodeButton.render(lang).is_empty());
        assert!(!Msg::AuthorButton.render(lang).is_empty());
        assert!(!Msg::JoinTestingButton.render(lang).is_empty());
        assert!(!Msg::GooglePlayButton.render(lang).is_empty());
        assert!(!Msg::DownloadApkButton.render(lang).is_empty());
        assert!(!Msg::AboutAndroidBeta1.render(lang).is_empty());
        assert!(!Msg::AboutAndroidBetaLink1.render(lang).is_empty());
        assert!(!Msg::AboutAndroidBeta2.render(lang).is_empty());
        assert!(!Msg::AboutAndroidBetaLink2.render(lang).is_empty());
        assert!(!Msg::AboutAndroidBeta3.render(lang).is_empty());
        assert!(!Msg::AboutAndroidBetaLink3.render(lang).is_empty());
        assert!(!Msg::AboutAndroidBeta4.render(lang).is_empty());
    }
    assert!(
        Msg::AboutAndroidBeta1
            .render(Language::Eng)
            .contains("closed beta")
    );
    assert!(
        Msg::AboutAndroidBeta1
            .render(Language::Spa)
            .contains("beta cerrada")
    );
    assert!(
        Msg::AboutAndroidBeta1
            .render(Language::Rus)
            .contains("бета")
    );
}

#[test]
fn build_year_is_four_digits() {
    assert!(FUNCTORA_CORE_YEAR.len() == 4);
    assert!(FUNCTORA_CORE_YEAR.bytes().all(|b| b.is_ascii_digit()));
}

#[test]
fn build_date_is_iso_8601() {
    let b = FUNCTORA_CORE_DATE.as_bytes();
    assert!(b.len() == 10);
    assert!(b[4] == b'-');
    assert!(b[7] == b'-');
    assert!(b[..4].iter().all(u8::is_ascii_digit));
    assert!(b[5..7].iter().all(u8::is_ascii_digit));
    assert!(b[8..10].iter().all(u8::is_ascii_digit));
}

#[test]
fn license_and_privacy_render_build_metadata() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert!(Msg::LicenseText.render(lang).contains(FUNCTORA_CORE_YEAR));
        assert!(Msg::PrivacyText.render(lang).contains(FUNCTORA_CORE_DATE));
    }
}
