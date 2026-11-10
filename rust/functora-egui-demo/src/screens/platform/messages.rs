use functora_egui::snippet;
use functora_egui::{Badge, Card, Flex, Typography};

impl crate::state::ShowcaseApp {
    pub fn demo_messages(&mut self, ui: &mut egui::Ui) {
        use functora_egui::i18n::I18N;
        _ = Typography::muted(
            "Messages / I18N: the full functora_core::messages::Msg catalog, the supported languages and the typed error variants, each rendered in every supported locale.",
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
            for lang in functora_egui::i18n::SUPPORTED_LANGUAGES {
                let _ = f.add(Badge::new(lang.to_string()));
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "language_from_code(\"es\"): {}, language_from_code(\"xx\"): {}",
            functora_egui::i18n::language_from_code("es"),
            functora_egui::i18n::language_from_code("xx")
        ))
        .show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "Ambient UI language (functora_egui::language)");
        let ctx = ui.ctx().clone();
        _ = Flex::row().gap(8.0).wrap().show(ui, |f| {
            for lang in functora_egui::i18n::SUPPORTED_LANGUAGES {
                if f.add(Badge::new(lang.to_string())).inner.clicked() {
                    functora_egui::language::set_language(&ctx, *lang);
                }
            }
        });
        ui.add_space(4.0);
        _ = Typography::small(format!(
            "current_language(ctx): {}",
            functora_egui::language::current_language(&ctx)
        ))
        .show(ui);
        _ = Typography::small(format!(
            "month_name(3, Eng/Spa/Rus): {} / {} / {}",
            functora_egui::language::month_name(3, functora_egui::i18n::Language::Eng),
            functora_egui::language::month_name(3, functora_egui::i18n::Language::Spa),
            functora_egui::language::month_name(3, functora_egui::i18n::Language::Rus)
        ))
        .show(ui);
        _ = Typography::small(format!(
            "weekday_short(0, Eng/Spa/Rus): {} / {} / {}",
            functora_egui::language::weekday_short(0, functora_egui::i18n::Language::Eng),
            functora_egui::language::weekday_short(0, functora_egui::i18n::Language::Spa),
            functora_egui::language::weekday_short(0, functora_egui::i18n::Language::Rus)
        ))
        .show(ui);
        ui.add_space(12.0);
        crate::screens::section(ui, "Message catalog (English)");
        _ = Card::new().show(ui, |ui2| {
            _ = Flex::column().gap(2.0).align_start().show(ui2, |f| {
                for message in Self::all_messages() {
                    _ = f.ui(|ui3| {
                        _ = Typography::small(format!("{message:?}: {}", message.render_eng()))
                            .show(ui3);
                    });
                }
            });
        });
        ui.add_space(12.0);
        crate::screens::section(ui, "Error variants");
        #[cfg(target_arch = "wasm32")]
        {
            _ = Typography::small(format!(
                "JsError (web only): {:?}",
                functora_egui::error::JsError(std::sync::Arc::new(
                    wasm_bindgen::JsValue::from_str("demo js failure")
                ))
            ))
            .show(ui);
        }
        _ = Card::new().show(ui, |ui2| {
            _ = Flex::column().gap(2.0).align_start().show(ui2, |f| {
                for error in Self::demo_errors() {
                    _ = f.ui(|ui3| {
                        _ = Typography::small(format!("{error:?}")).show(ui3);
                    });
                }
            });
        });
        ui.add_space(12.0);
        snippet(
            ui,
            "// Messages / I18N: the same value rendered in every supported language\nuse functora_egui::error::Error;\nuse functora_egui::i18n::{I18N, Language, SUPPORTED_LANGUAGES, language_from_code};\nuse functora_egui::messages::Msg;\n\nlet err = Error::JS(\"demo error\".into());\n\n// One value, three locales\nassert!(!err.render_eng().is_empty());\nassert!(!err.render_spa().is_empty());\nassert!(!err.render_rus().is_empty());\n\n// Supported UI languages\nfor lang in SUPPORTED_LANGUAGES {\n    eprintln!(\"{lang}\");\n}\nassert_eq!(language_from_code(\"es\"), Language::Spa);\n\n// The ambient language every widget follows\nuse functora_egui::language::{current_language, month_name, set_language, weekday_short};\n\nset_language(ctx, Language::Spa);\nassert_eq!(current_language(ctx), Language::Spa);\nassert_eq!(month_name(3, Language::Eng), \"March\");\nassert_eq!(weekday_short(0, Language::Eng), \"Mo\");\n\n// Every message the library can produce\nlet messages = vec![\n    Msg::Copied,\n    Msg::Pasted,\n    Msg::ErrorTitle(\"boom\".to_owned()),\n    Msg::EnvError(std::env::VarError::NotPresent),\n    Msg::ChannelError(std::sync::mpsc::RecvError),\n    Msg::NotJsonObject(\"[]\".to_owned()),\n    Msg::Stage(functora_egui::progress::Stage::Encrypt),\n    Msg::FilesAttached(3),\n];\nfor message in messages {\n    eprintln!(\"{}\", message.render_eng());\n}\n\n// Typed error variants\nuse functora_egui::error::{Capability, ClipboardError, IoError, JsonError, Missing, WorkerStopped, ZipErr};\n\nlet errors = vec![\n    Error::Missing(Missing::Window),\n    Error::CapabilityDisabled(Capability::Clipboard),\n    Error::Clipboard(ClipboardError::Busy),\n    Error::Worker(WorkerStopped),\n    Error::IO(IoError::from(std::io::Error::other(\"demo io failure\"))),\n    Error::Json(JsonError::from(serde_json::Error::io(std::io::Error::other(\"demo json failure\")))),\n];\nfor error in errors {\n    eprintln!(\"{error}\");\n}\n// ZipErr wraps the archive failure, JniError the Android bridge failure,\n// JsError the rejected Web API value (wasm only)",
        );
    }
}
