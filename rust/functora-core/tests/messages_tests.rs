use functora_core::Msg;
use functora_core::i18n::{I18N, Language};

#[test]
fn pick_feedback_renders_all_languages() {
    for lang in [Language::Eng, Language::Spa, Language::Rus] {
        assert!(!Msg::PickingFiles.render(lang).is_empty());
        let attached = Msg::FilesAttached(2).render(lang);
        assert!(attached.contains('2'), "count must render: {attached}");
        assert!(!attached.is_empty());
    }
}

#[test]
fn widget_chrome_renders_all_languages() {
    let variants = [
        Msg::Cancel,
        Msg::Start,
        Msg::Stop,
        Msg::PickImage,
        Msg::Clear,
        Msg::Preparing,
        Msg::Cancelling,
        Msg::CameraStarting,
        Msg::CameraOff,
        Msg::Decoded,
    ];
    for variant in variants {
        for lang in [Language::Eng, Language::Spa, Language::Rus] {
            assert!(
                !variant.render(lang).is_empty(),
                "empty translation for {variant:?} in {lang:?}"
            );
        }
    }
}
