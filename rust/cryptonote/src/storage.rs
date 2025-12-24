use crate::i18n::{Language, detect_browser_language};
use crate::js::Theme;
use crate::prelude::*;
pub mod mobile;

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq,
)]
pub struct AppCfg {
    pub theme: Theme,
    pub language: Language,
}

impl Default for AppCfg {
    fn default() -> Self {
        Self {
            theme: Theme::Light,
            language: detect_browser_language(),
        }
    }
}
