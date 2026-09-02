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
#[serde(bound(serialize = "Extra: Serialize", deserialize = "Extra: serde::de::DeserializeOwned"))]
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
    Extra: Clone + PartialEq + Default,
{
    fn default() -> Self {
        Self {
            theme: Theme::Light,
            language: crate::i18n::detect_browser_language(),
            extra: Extra::default(),
        }
    }
}

impl<Extra> PersistentState<Extra>
where
    Extra: Clone + PartialEq + Default,
{
    #[must_use]
    pub fn with_system_defaults(ctx: &egui::Context, extra: Extra) -> Self {
        Self {
            theme: functora_egui::theme_extra::default_theme(ctx),
            language: crate::i18n::detect_browser_language(),
            extra,
        }
    }

    #[must_use]
    pub fn load_or_default(ctx: &egui::Context, key: &str, extra: Extra) -> Self
    where
        Extra: Serialize + serde::de::DeserializeOwned,
    {
        load_state::<Self>(key).unwrap_or_else(|| Self::with_system_defaults(ctx, extra))
    }
}
