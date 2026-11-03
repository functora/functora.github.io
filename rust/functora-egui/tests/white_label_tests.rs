use functora_egui::LucideIcon;
use functora_egui::i18n::Language;
use functora_egui::white_label::{DEFAULT_SIDEBAR_ICONS, default_sidebar_labels};

#[test]
fn default_sidebar_labels_are_i18n() {
    let expected = [
        (Language::Eng, ["Home", "About", "Donate", "Reset"]),
        (
            Language::Spa,
            ["Inicio", "Acerca de", "Donar", "Restablecer"],
        ),
        (
            Language::Rus,
            ["Главная", "О приложении", "Пожертвовать", "Сбросить"],
        ),
    ];
    for (lang, expected_labels) in expected {
        let labels = default_sidebar_labels(lang);
        assert_eq!(labels.len(), 4, "sidebar must have 4 entries for {lang:?}");
        for (got, want) in labels.iter().zip(expected_labels) {
            assert_eq!(
                got, want,
                "sidebar label mismatch for {lang:?}: got {got:?} want {want:?}"
            );
        }
        assert!(
            labels.iter().all(|label| !label.is_empty()),
            "no empty sidebar labels for {lang:?}"
        );
    }
}

#[test]
fn default_sidebar_icons_are_fixed() {
    assert_eq!(
        DEFAULT_SIDEBAR_ICONS,
        [
            LucideIcon::House,
            LucideIcon::Info,
            LucideIcon::Heart,
            LucideIcon::RotateCcw,
        ]
    );
}
