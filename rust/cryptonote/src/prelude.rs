pub use derive_more::Display;
pub use dioxus::prelude::*;
pub use dioxus_free_icons::icons::fa_brands_icons::FaAndroid;
pub use dioxus_free_icons::icons::fa_solid_icons::{
    FaArrowLeft, FaCopy, FaDownload, FaEye, FaFolderOpen, FaLock, FaLockOpen, FaPaperclip, FaPaste, FaPenToSquare,
    FaPrint, FaQrcode, FaShareNodes, FaSquarePlus, FaTrash, FaXmark,
};
pub use dioxus_free_icons::{Icon, IconShape};
pub use functora::*;
pub use functora_dioxus::dioxus_elements;
pub use serde::{Deserialize, Serialize};
pub use std::str::FromStr;
pub use std::sync::Arc;
pub use thiserror::Error;

pub use functora_dioxus::ffi::{print_page, social_share, ShareData, Theme};
pub use functora_dioxus::storage::PersistentSignal;
pub use functora_dioxus::widgets::JobProgressBar;
pub use functora_dioxus::Msg as BaseMsg;
pub use functora_dioxus::{use_nav, Nav};
pub use functora_dioxus::{AppAssets, AppAttrs, InfallibleInto};

pub const APP_ATTRS: AppAttrs = AppAttrs {
    app: env!("CARGO_PKG_NAME"),
    vsn: env!("CARGO_PKG_VERSION"),
    org: "functora",
    src: Some("rust"),
    dst: "apps",
};
