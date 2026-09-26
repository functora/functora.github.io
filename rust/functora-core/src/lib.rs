#[cfg(feature = "crypto")]
pub mod crypto;
pub mod deep_link;
pub mod encoding;
pub mod error;
pub mod files;
pub mod i18n;
#[cfg(feature = "markdown")]
pub mod markdown;
pub mod messages;
#[cfg(feature = "package")]
pub mod package;
pub mod progress;
pub mod qr;
pub mod storage;
#[cfg(feature = "thumbnail")]
pub mod thumbnail;
pub mod white_label;
pub mod worker;
#[cfg(feature = "zip")]
pub mod zip;

#[cfg(feature = "crypto")]
pub use crypto::*;
pub use deep_link::*;
pub use encoding::*;
pub use error::*;
pub use files::*;
pub use i18n::*;
#[cfg(feature = "markdown")]
pub use markdown::*;
pub use messages::*;
#[cfg(feature = "package")]
pub use package::*;
pub use progress::*;
pub use qr::*;
pub use storage::*;
#[cfg(feature = "thumbnail")]
pub use thumbnail::*;
pub use white_label::*;
pub use worker::*;
#[cfg(feature = "zip")]
pub use zip::*;

pub const FUNCTORA_CORE_YEAR: &str = env!("FUNCTORA_CORE_YEAR");
pub const FUNCTORA_CORE_DATE: &str = env!("FUNCTORA_CORE_DATE");
const _: () = assert!(
    !FUNCTORA_CORE_YEAR.is_empty(),
    "FUNCTORA_CORE_YEAR must not be empty"
);
const _: () = assert!(
    !FUNCTORA_CORE_DATE.is_empty(),
    "FUNCTORA_CORE_DATE must not be empty"
);
