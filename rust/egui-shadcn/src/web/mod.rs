pub mod config;
pub mod startup;
pub mod templates;

#[cfg(target_arch = "wasm32")]
pub mod runner;

pub use config::{derive_pkg_js, derive_theme_color, derive_title, load_config, WebConfig};
pub use startup::startup_width;
