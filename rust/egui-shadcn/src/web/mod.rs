#[cfg(any(feature = "web", feature = "build"))]
pub mod config;
#[cfg(all(feature = "web", target_arch = "wasm32"))]
pub mod runner;
#[cfg(feature = "web")]
pub mod startup;
#[cfg(any(feature = "web", feature = "build"))]
pub mod templates;

#[cfg(any(feature = "web", feature = "build"))]
pub use config::{derive_pkg_js, derive_theme_color, derive_title, load_config, WebConfig};
#[cfg(feature = "web")]
pub use startup::startup_width;
