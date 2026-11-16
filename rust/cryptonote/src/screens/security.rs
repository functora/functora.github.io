use std::sync::LazyLock;

use crate::app::CryptonoteApp;
use crate::markdown::tables_to_lists;
use functora_egui::i18n::Language;

/// The shipped security policy, rendered in the app so the threat model is
/// readable where the notes are created. The same file is the `SECURITY.md`
/// in the crate root, so the document never drifts from the repository.
static POLICY_EN: LazyLock<String> = LazyLock::new(|| tables_to_lists(include_str!("../../SECURITY.md")));

/// The Spanish security policy, the translation of `SECURITY.md`.
static POLICY_ES: LazyLock<String> = LazyLock::new(|| tables_to_lists(include_str!("../../SECURITY.es.md")));

/// The Russian security policy, the translation of `SECURITY.md`.
static POLICY_RU: LazyLock<String> = LazyLock::new(|| tables_to_lists(include_str!("../../SECURITY.ru.md")));

/// The security policy in the language of the interface.
fn policy(lang: Language) -> &'static str {
    match lang {
        Language::Spa => POLICY_ES.as_str(),
        Language::Rus => POLICY_RU.as_str(),
        _ => POLICY_EN.as_str(),
    }
}

impl CryptonoteApp {
    pub(crate) fn screen_security(&mut self, ui: &mut egui::Ui) {
        let lang = self.lang();
        _ = functora_egui::markdown_view::show(ui, &mut self.md_cache, policy(lang));
        self.dock_back(ui);
    }
}
