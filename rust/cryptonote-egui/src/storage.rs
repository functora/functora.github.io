pub use functora_egui::state::PersistentState;
pub use functora_egui::storage::{Persistent, Storage};

pub(crate) const APP_SCOPE: &str = "cryptonote-egui";

use functora_egui::white_label::AppAttrs;

pub const APP_ATTRS: AppAttrs = AppAttrs {
    app: env!("CARGO_PKG_NAME"),
    vsn: env!("CARGO_PKG_VERSION"),
    org: "functora",
    src: Some("rust"),
    dst: "apps",
    description: "Cryptonote is a cross-platform, serverless app for encrypted offline notes.",
};
