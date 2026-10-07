pub mod config;
#[cfg(any(feature = "build", feature = "desktop"))]
pub mod icons;
#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
#[cfg(feature = "desktop")]
pub mod run;
#[cfg(any(feature = "build", feature = "desktop"))]
pub mod templates;

pub use config::{DesktopConfig, load_desktop_config};
#[cfg(feature = "images")]
#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
#[cfg(feature = "desktop")]
pub use run::icon_from_png;
#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
#[cfg(feature = "desktop")]
pub use run::{ingest_launch_args, launch_args, run, run_with_icon, take_file_args};
