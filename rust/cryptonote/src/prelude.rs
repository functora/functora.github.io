pub use derive_more::Display;
pub use dioxus::prelude::*;
pub use dioxus_free_icons::icons::fa_brands_icons::{FaAndroid, FaGithub, FaGoogle, FaGooglePlay};
pub use dioxus_free_icons::icons::fa_solid_icons::{
    FaArrowLeft, FaCopy, FaDownload, FaEye, FaFolderOpen, FaHeart, FaLock, FaLockOpen, FaPaste,
    FaPenToSquare, FaQrcode, FaShareNodes, FaSquarePlus, FaTrash, FaUser,
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

pub use functora_dioxus::ffi::Theme;
pub use functora_dioxus::storage::{use_storage, PersistentSignal};
pub use functora_dioxus::Msg as BaseMsg;
pub use functora_dioxus::{use_nav, Align, Nav, Par};
pub use functora_dioxus::{AppAssets, AppId, AppName, InfallibleInto};

pub const APP_NAME: &str = env!("CARGO_PKG_NAME");
pub const APP_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APP_ID: &str = concat!(env!("CARGO_PKG_NAME"), "-", env!("CARGO_PKG_VERSION"));
pub const BETA_TEST_URL: &str = "https://groups.google.com/g/functora";
pub const GOOGLE_PLAY_URL: &str = concat!(
    "https://play.google.com/store/apps/details?id=com.functora.",
    env!("CARGO_PKG_NAME")
);
pub const APK_URL: &str = concat!(
    "https://github.com/functora/functora.github.io/releases/tag/",
    env!("CARGO_PKG_NAME"),
    "-v",
    env!("CARGO_PKG_VERSION")
);
pub const WEB_APP_URL: &str = concat!(
    "https://functora.github.io/apps/",
    env!("CARGO_PKG_NAME"),
    "/",
    env!("CARGO_PKG_VERSION")
);
pub const FUNCTORA_URL: &str = "https://functora.github.io/";
pub const SOURCE_CODE_URL: &str = concat!(
    "https://github.com/functora/functora.github.io/tree/master/rust/",
    env!("CARGO_PKG_NAME")
);
