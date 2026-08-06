use crate::ffi::Theme;
use crate::i18n::Language;
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
            language: crate::i18n::detect_browser_language(),
        }
    }
}
