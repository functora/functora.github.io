pub mod config;
#[cfg(target_os = "android")]
pub mod ime;
#[cfg(target_os = "android")]
pub mod intent;
#[cfg(target_os = "android")]
pub mod run;
#[cfg(any(feature = "build", feature = "android"))]
pub mod templates;

pub use config::{load_android_config, AndroidConfig};
#[cfg(target_os = "android")]
pub use ime::poll_ime;
#[cfg(target_os = "android")]
pub use intent::get_data_string;
#[cfg(target_os = "android")]
pub use run::run;
