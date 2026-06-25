pub use derive_more::Display;
pub use dioxus::prelude::*;
pub use dioxus_free_icons::icons::fa_brands_icons::{
    FaAndroid, FaGithub, FaGoogle, FaGooglePlay,
};
pub use dioxus_free_icons::icons::fa_solid_icons::{
    FaArrowLeft, FaCopy, FaDownload, FaEye, FaFolderOpen,
    FaHeart, FaLock, FaLockOpen, FaPaste, FaPenToSquare,
    FaQrcode, FaShareNodes, FaSquarePlus, FaTrash, FaUser,
};
pub use dioxus_free_icons::{Icon, IconShape};
pub use either::*;
pub use enum_iterator::{next_cycle, Sequence};
pub use functora::*;
pub use functora_dioxus::dioxus_elements;
pub use serde::de::DeserializeOwned;
pub use serde::{Deserialize, Serialize};
pub use std::str::FromStr;
pub use thiserror::Error;

pub use functora_dioxus::js::Theme;
pub use functora_dioxus::storage::{
    use_storage, PersistentSignal,
};
pub use functora_dioxus::{use_nav, Nav};

pub const APP_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APP_STORAGE_KEY: &str = concat!(
    "cryptonote-",
    env!("CARGO_PKG_VERSION"),
    "-cfg"
);
pub const BETA_TEST_URL: &str =
    "https://groups.google.com/g/functora";
pub const GOOGLE_PLAY_URL: &str =
    "https://play.google.com/store/apps/details?id=com.functora.cryptonote";
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
pub const SOURCE_CODE_URL: &str =
    "https://github.com/functora/functora.github.io/tree/master/rust/cryptonote";
