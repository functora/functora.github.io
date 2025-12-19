use crate::i18n::{Language, detect_browser_language};
use crate::js::Theme;
use crate::prelude::*;

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq,
)]
pub struct AppSettings {
    pub theme: Theme,
    pub language: Language,
}

impl Default for AppSettings {
    fn default() -> Self {
        Self {
            theme: Theme::Light,
            language: detect_browser_language(),
        }
    }
}
