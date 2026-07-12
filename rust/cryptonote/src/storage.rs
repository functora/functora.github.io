use crate::i18n::Language;
use crate::prelude::Theme;
use dioxus::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Store, Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct PersistentState {
    pub theme: Theme,
    pub language: Language,
}

impl Default for PersistentState {
    fn default() -> Self {
        Self {
            theme: Theme::Light,
            language: functora_dioxus::i18n::detect_browser_language(),
        }
    }
}
