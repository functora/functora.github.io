pub use functora_egui::state::PersistentState;
pub use functora_egui::storage::{Persistent, Storage};

pub(crate) const APP_SCOPE: &str = "cryptonote";

use functora_egui::white_label::{AppAttrs, functora_attrs};

pub const APP_ATTRS: AppAttrs = functora_attrs(
    env!("CARGO_PKG_NAME"),
    env!("CARGO_PKG_VERSION"),
    "Cryptonote is a cross-platform, serverless app for encrypted offline notes.",
);
