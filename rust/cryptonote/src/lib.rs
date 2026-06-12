pub mod components;
pub mod crypto;
pub mod encoding;
pub mod error;
pub mod hooks;
pub mod i18n;
pub mod markdown;
pub mod messages;
pub mod prelude;

pub use components::*;
pub use crypto::*;
pub use encoding::*;
pub use error::*;
pub use hooks::*;
pub use i18n::*;
pub use markdown::*;
pub use prelude::*;

pub use functora_dioxus::clipboard::write_clipboard;
pub use functora_dioxus::js::*;

pub mod qr_decode {
    pub use functora_dioxus::qr::*;
}
pub use qr_decode::*;

pub use storage::*;
pub mod storage {
    pub mod mobile {
        pub use functora_dioxus::storage::mobile::*;
    }

    use functora_dioxus::i18n::detect_browser_language;
    use functora_dioxus::i18n::Language;
    use functora_dioxus::js::Theme;
    use serde::{Deserialize, Serialize};

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
}
