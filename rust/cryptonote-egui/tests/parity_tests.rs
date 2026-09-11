//! Content parity with `cryptonote` (dioxus): no hardcoded English UI text
//! in `screens`/shell, full `WhiteLabel` footer sentence, and complete
//! About/Donate content. Mirrors the dioxus screen/message catalog.

#![allow(clippy::unwrap_used, clippy::expect_used)]

const APP_SRC: &str = include_str!("../src/app.rs");
const HOME_SRC: &str = include_str!("../src/screens/home.rs");
const OPEN_SRC: &str = include_str!("../src/screens/open.rs");
const VIEW_SRC: &str = include_str!("../src/screens/view.rs");
const SHARE_SRC: &str = include_str!("../src/screens/share.rs");
const FILE_SRC: &str = include_str!("../src/screens/file.rs");
const ABOUT_SRC: &str = include_str!("../src/screens/about.rs");
const DONATE_SRC: &str = include_str!("../src/screens/donate.rs");
const LICENSE_SRC: &str = include_str!("../src/screens/license.rs");
const PRIVACY_SRC: &str = include_str!("../src/screens/privacy.rs");
const ROUTE_SRC: &str = include_str!("../src/route.rs");

fn ui_sources() -> [&'static str; 11] {
    [
        APP_SRC,
        HOME_SRC,
        OPEN_SRC,
        VIEW_SRC,
        SHARE_SRC,
        FILE_SRC,
        ABOUT_SRC,
        DONATE_SRC,
        LICENSE_SRC,
        PRIVACY_SRC,
        ROUTE_SRC,
    ]
}

/// User-visible English literals that must come from the trilingual catalog
/// instead. Each entry names the replacement.
const BANNED: &[(&str, &str)] = &[
    ("\"Pasted\"", "Base::Copied"),
    ("\"Copied\"", "Base::Copied"),
    ("\"Shared\"", "Msg::Sent"),
    ("Copied for printing", "drop Print"),
    ("Copied URL for printing", "drop Print"),
    ("\"Dismiss\"", "Base::Dismiss"),
    ("\"No encryption\"", "Msg::NoEncryption"),
    ("AES-256-GCM", "Base::CipherAesLabel"),
    ("ChaCha20-Poly1305", "Base::CipherChaChaLabel"),
    ("Continuous: ON", "always-continuous scan"),
    ("Continuous: OFF", "always-continuous scan"),
    ("\"Markdown preview (raw HTML)\"", "CommonMarkViewer"),
    ("\"QR Code (SVG):\"", "QR image without label"),
    ("\"Tap to copy URL\"", "bare readonly URL"),
    (
        "\"Support Cryptonote development:\"",
        "Base::DonateGreeting/DonateIntro",
    ),
    ("MIT License - see https://github.com", "Base::LicenseText"),
    ("fully offline", "Base::PrivacyText"),
    ("free and open source", "WhiteLabel footer only"),
    ("no-op, footer link", "working footer navigation"),
    ("preview blob available", "inline image"),
    ("\"Video: \"", "PreviewUnavailable fallback"),
    ("\"Audio: \"", "PreviewUnavailable fallback"),
    ("\"PDF: \"", "PreviewUnavailable fallback"),
    ("can't render SVG directly", "QR image"),
    ("\"Open\"", "Msg::ViewButton/Msg::OpenButton"),
    ("\"Download\"", "Msg::Download"),
    ("\"Copy\"", "Base::Copy"),
    ("\"Password\"", "Base::Password/PasswordPlaceholder"),
    ("\"File\"", "Msg::File"),
    ("\"View\"", "Msg::ViewButton"),
    ("\"Share\"", "Msg::Share"),
    ("\"About\"", "Base::Application"),
    ("\"Donate\"", "Base::Donate"),
    ("\"License\"", "Base::TermsOfServiceTitle"),
    ("\"Privacy\"", "Base::PrivacyPolicyTitle"),
    ("\"Back\"", "Base::Back"),
    ("\"Reset\"", "Msg::CreateNewNote"),
    ("\"Home\"", "Base::Home"),
    ("\"Paste\"", "Base::Paste"),
];

#[test]
fn no_hardcoded_ui_strings() {
    for (banned, replacement) in BANNED {
        for (idx, src) in ui_sources().iter().enumerate() {
            assert!(
                !src.contains(banned),
                "banned literal {banned:?} in source {idx} (use {replacement})"
            );
        }
    }
}

#[test]
fn footer_matches_whitelabel_sentence() {
    // Note: dioxus links Share to About+anchor (`share_anchor_id`) for smooth
    // scroll; egui has no scroll anchors, so the footer navigates to the
    // About screen, which hosts the same share section.
    for fragment in [
        "AllRightsReserved",
        "ByContinuing",
        "TermsOfService",
        "PrivacyPolicyAnd",
        "DonateLink",
        "FooterShareWord",
        "FooterAppWord",
        "VersionLabel",
    ] {
        assert!(
            APP_SRC.contains(fragment),
            "footer must render WhiteLabel fragment {fragment}"
        );
    }
}

#[test]
fn footer_is_one_uniform_hypertext_layout() {
    // Mixed labels, buttons and hyperlinks in one row render at different
    // sizes and baselines; a single Hypertext layout stays uniform.
    assert!(
        APP_SRC.contains("show_action"),
        "footer must use Hypertext::show_action for navigation"
    );
    assert!(
        !APP_SRC.contains("horizontal_wrapped"),
        "footer must not hand-roll a mixed-widget row"
    );
    assert!(
        !APP_SRC.contains("footer_link"),
        "footer must not use per-link buttons"
    );
    assert!(
        APP_SRC.contains(".centered()"),
        "footer paragraph must be centered"
    );
}

#[test]
fn about_screen_has_full_content() {
    for fragment in [
        "AboutText",
        "AboutAndroidBeta1",
        "beta_url",
        "google_play_url",
        "apk_url",
        "CopyAppLink",
        "ShareAppLink",
        "SourceCodeButton",
        "AuthorButton",
        "JoinTestingButton",
        "GooglePlayButton",
        "DownloadApkButton",
        "from_bytes",
    ] {
        assert!(ABOUT_SRC.contains(fragment), "about screen must include {fragment}");
    }
}

#[test]
fn donate_screen_uses_greeting_and_blocks() {
    for fragment in ["DonateGreeting", "DonateIntro", "donate_blocks", "generate_qr_code"] {
        assert!(DONATE_SRC.contains(fragment), "donate screen must include {fragment}");
    }
}

#[test]
fn license_and_privacy_use_full_texts() {
    assert!(
        LICENSE_SRC.contains("LicenseText"),
        "license screen must render Base::LicenseText"
    );
    assert!(
        PRIVACY_SRC.contains("PrivacyText"),
        "privacy screen must render Base::PrivacyText"
    );
}

#[test]
fn route_labels_are_localized() {
    assert!(
        !ROUTE_SRC.contains("\"Home\""),
        "route labels must render from the catalog, not literals"
    );
    for fragment in ["TermsOfServiceTitle", "PrivacyPolicyTitle", "OpenButton", "ViewButton"] {
        assert!(ROUTE_SRC.contains(fragment), "route labels must include {fragment}");
    }
}
