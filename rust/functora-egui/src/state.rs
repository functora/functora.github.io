use crate::theme_extra::Theme;
use functora_core::i18n::{Language, detect_browser_language};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(bound(
    serialize = "Extra: Serialize",
    deserialize = "Extra: DeserializeOwned"
))]
pub struct PersistentState<Extra = ()>
where
    Extra: Clone + PartialEq + Default,
{
    pub theme: Theme,
    pub language: Language,
    #[serde(default)]
    pub extra: Extra,
}

impl<Extra> Default for PersistentState<Extra>
where
    Extra: Clone + PartialEq + Serialize + DeserializeOwned + Default,
{
    fn default() -> Self {
        Self {
            theme: Theme::Light,
            language: detect_browser_language(),
            extra: Extra::default(),
        }
    }
}

impl<Extra> PersistentState<Extra>
where
    Extra: Clone + PartialEq + Serialize + DeserializeOwned + Default,
{
    #[must_use]
    pub fn with_system_defaults(ctx: &egui::Context, extra: Extra) -> Self {
        Self {
            theme: crate::theme_extra::default_theme(ctx),
            language: detect_browser_language(),
            extra,
        }
    }

    #[must_use]
    pub fn load_or_default(ctx: &egui::Context, key: &str, extra: Extra) -> Self {
        crate::storage::load_state::<Self>(key)
            .unwrap_or_else(|| Self::with_system_defaults(ctx, extra))
    }

    #[must_use]
    pub fn load_or_default_no_ctx(key: &str, extra: Extra) -> Self {
        crate::storage::load_state::<Self>(key).unwrap_or_else(|| Self {
            theme: Theme::Light,
            language: detect_browser_language(),
            extra,
        })
    }
}
