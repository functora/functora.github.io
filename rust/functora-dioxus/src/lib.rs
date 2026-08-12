#[cfg(target_os = "android")]
pub mod android;
pub mod app;
pub mod crypto;
pub mod deep_link;
pub mod dioxus_elements;
pub mod encoding;
pub mod error;
pub mod ffi;
pub mod files;
pub mod hooks;
pub mod i18n;
pub mod markdown;
pub mod messages;
pub mod nav;
pub mod package;
pub mod progress;
pub mod qr;
pub mod screens;
pub mod state;
pub mod storage;
pub mod thumbnail;
pub mod white_label;
pub mod widgets;
pub mod worker;
pub mod zip;

pub const FUNCTORA_DIOXUS_YEAR: &str = env!("FUNCTORA_DIOXUS_YEAR");
pub const FUNCTORA_DIOXUS_DATE: &str = env!("FUNCTORA_DIOXUS_DATE");
const _: () = assert!(
    !FUNCTORA_DIOXUS_YEAR.is_empty(),
    "FUNCTORA_DIOXUS_YEAR must not be empty"
);
const _: () = assert!(
    !FUNCTORA_DIOXUS_DATE.is_empty(),
    "FUNCTORA_DIOXUS_DATE must not be empty"
);

pub use app::*;
pub use crypto::*;
pub use deep_link::*;
pub use encoding::*;
pub use error::*;
pub use ffi::*;
pub use files::*;
pub use hooks::*;
pub use i18n::*;
pub use markdown::*;
pub use messages::*;
pub use nav::*;
pub use package::*;
pub use progress::*;
pub use qr::*;
pub use screens::*;
pub use state::*;
pub use storage::*;
pub use white_label::*;
pub use widgets::*;
pub use worker::*;
pub use zip::*;
