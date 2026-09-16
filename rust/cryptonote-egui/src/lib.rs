pub mod app;
pub mod archive;
pub mod crypto;
pub mod deep_link;
pub mod encoding;
pub mod error;
pub mod hooks;
pub mod i18n;
pub mod markdown;
pub mod messages;
pub mod progress;
pub mod route;
pub mod screens;
pub mod state;
pub mod storage;
#[cfg(target_arch = "wasm32")]
pub mod web;
pub mod worker;

pub use app::CryptonoteApp;
pub use archive::*;
pub use crypto::*;
pub use deep_link::*;
pub use encoding::*;
pub use error::*;
pub use hooks::*;
pub use i18n::*;
pub use markdown::*;
pub use messages::*;
pub use progress::*;
pub use route::Screen;
pub use state::{ActionMode, CipherChoice, External, ExternalNote, TemporaryState};
pub use storage::{APP_ATTRS, PersistentState};
pub use worker::*;

pub mod qr_decode {
    pub use functora_egui::qr::*;
}
pub use qr_decode::*;

#[cfg(target_os = "android")]
pub(crate) mod android {
    use android_activity::AndroidApp;

    #[unsafe(export_name = "android_main")]
    pub fn android_main(app: AndroidApp) {
        functora_egui::android::run(app, "Cryptonote", |cc| {
            Ok(Box::new(crate::CryptonoteApp::new(cc)) as Box<dyn eframe::App>)
        });
    }
}
