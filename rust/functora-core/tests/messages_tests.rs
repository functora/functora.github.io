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
