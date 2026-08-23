use crate::theme_extra::Theme;
use functora_core::i18n::{Language, detect_browser_language};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PersistentState {
    pub theme: Theme,
    pub language: Language,
}

impl Default for PersistentState {
    fn default() -> Self {
        Self {
            theme: Theme::Light,
            language: detect_browser_language(),
        }
    }
}
