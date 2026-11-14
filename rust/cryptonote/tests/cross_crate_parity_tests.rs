const DONATE_SRC: &str = include_str!("../src/screens/donate.rs");
const CORE_WHITE_LABEL_SRC: &str = include_str!("../../functora-core/src/white_label.rs");
const WHITE_LABEL_SRC: &str = include_str!("../../functora-egui/src/white_label.rs");

#[test]
fn donate_screen_uses_greeting_and_blocks() {
    for fragment in ["DonateGreeting", "DonateIntro"] {
        assert!(DONATE_SRC.contains(fragment), "donate screen must include {fragment}");
    }
    for fragment in ["donate_blocks", "QrImage", "TextareaPasteClear", "readonly()"] {
        assert!(
            WHITE_LABEL_SRC.contains(fragment),
            "white label donate screen must include {fragment}"
        );
    }
}

#[test]
fn privacy_renders_subheadings_as_markdown() {
    assert!(
        WHITE_LABEL_SRC.contains("markdown_view"),
        "license and privacy screens must render via markdown_view so sub-headings show as headings"
    );
}

#[test]
fn license_and_privacy_use_full_texts() {
    assert!(
        CORE_WHITE_LABEL_SRC.contains("MIT License"),
        "white label default license text must render the full MIT license"
    );
    assert!(
        CORE_WHITE_LABEL_SRC.contains("Privacy Policy"),
        "white label default privacy text must render the full privacy policy"
    );
}
