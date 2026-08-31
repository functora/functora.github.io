pub use functora_egui::storage::{Persistent, load_state, persist_value};

use serde::{Deserialize, Serialize};

use crate::i18n::Language;
use functora_egui::theme_extra::Theme;
use functora_egui::white_label::AppAttrs;

pub const APP_ATTRS: AppAttrs = AppAttrs {
    app: env!("CARGO_PKG_NAME"),
    vsn: env!("CARGO_PKG_VERSION"),
    org: "functora",
    src: Some("rust"),
    dst: "apps",
    description: "Cryptonote is a cross-platform, serverless app for encrypted offline notes.",
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PersistentState {
    pub theme: Theme,
    pub language: Language,
}

impl Default for PersistentState {
    fn default() -> Self {
        Self {
            theme: Theme::Light,
            language: crate::i18n::detect_browser_language(),
        }
    }
}
