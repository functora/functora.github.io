use functora_egui::snippet;
use functora_egui::{Badge, Card, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_messages(ui: &mut egui::Ui) {
        use functora_egui::i18n::I18N;
        _ = Typography::muted(
            "Messages / I18N: functora_core::messages + i18n Language (Eng/Spa/Rus). Error::render_* etc.",
        )
        .show(ui);
        ui.add_space(12.0);
        let err = functora_egui::error::Error::JS("demo error".into());
        _ = Card::new().show(ui, |ui2| {
            _ = Typography::small(format!("EN: {}", err.render_eng())).show(ui2);
            _ = Typography::small(format!("SPA: {}", err.render_spa())).show(ui2);
            _ = Typography::small(format!("RU: {}", err.render_rus())).show(ui2);
        });
        ui.add_space(8.0);
        _ = Flex::row().gap(8.0).show(ui, |f| {
            for lang in [
                functora_egui::i18n::Language::Eng,
                functora_egui::i18n::Language::Spa,
                functora_egui::i18n::Language::Rus,
            ] {
                let _ = f.add(Badge::new(lang.to_string()));
            }
        });
        ui.add_space(12.0);
        snippet(
            ui,
            "// Messages / I18N: the same value rendered in every supported language\nuse functora_egui::error::Error;\nuse functora_egui::i18n::{I18N, Language};\n\nlet err = Error::JS(\"demo error\".into());\n\n// One value, three locales\nassert!(!err.render_eng().is_empty());\nassert!(!err.render_spa().is_empty());\nassert!(!err.render_rus().is_empty());\n\n// Supported UI languages\nfor lang in [Language::Eng, Language::Spa, Language::Rus] {\n    eprintln!(\"{lang}\");\n}",
        );
    }
}
