pub use derive_more::Display;
pub use dioxus::core::SpawnIfAsync;
pub use dioxus::document::EvalError;
pub use dioxus::prelude::*;
pub use dioxus_free_icons::icons::fa_brands_icons::{
    FaAndroid, FaGithub, FaGoogle, FaGooglePlay,
};
pub use dioxus_free_icons::icons::fa_solid_icons::{
    FaArrowLeft, FaCopy, FaDownload, FaFolderOpen, FaHeart,
    FaLock, FaLockOpen, FaPenToSquare, FaShareNodes,
    FaSquarePlus, FaTrash, FaUser,
};
pub use dioxus_free_icons::{Icon, IconShape};
pub use either::*;
pub use enum_iterator::{Sequence, next_cycle};
pub use functora::*;
pub use serde::de::DeserializeOwned;
pub use serde::{Deserialize, Serialize};
pub use std::fmt::Display;
pub use std::str::FromStr;
pub mod dioxus_elements {
    pub use elements::*;
    pub mod elements {
        pub use dioxus::prelude::dioxus_elements::*;
        pub mod card {
            pub use dioxus::prelude::dioxus_elements::elements::template::*;
            pub const TAG_NAME: &str = "card";
            pub const NAME_SPACE: Option<&'static str> =
                None;
        }
    }
}

pub const APP_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APP_STORAGE_KEY: &str = concat!(
    "cryptonote-",
    env!("CARGO_PKG_VERSION"),
    "-cfg"
);
pub const BETA_TEST_URL: &str =
    "https://groups.google.com/g/functora";
pub const GOOGLE_PLAY_URL: &str = "https://play.google.com/store/apps/details?id=com.functora.cryptonote";
pub const APK_URL: &str = concat!(
    "https://github.com/functora/functora.github.io/releases/tag/cryptonote-v",
    env!("CARGO_PKG_VERSION")
);
pub const WEB_APP_URL: &str = concat!(
    "https://functora.github.io/apps/cryptonote/",
    env!("CARGO_PKG_VERSION")
);
pub const FUNCTORA_URL: &str =
    "https://functora.github.io/";
pub const SOURCE_CODE_URL: &str = "https://github.com/functora/functora.github.io/tree/master/rust/cryptonote";
