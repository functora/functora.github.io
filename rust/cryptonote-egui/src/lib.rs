pub mod app;
pub mod archive;
pub mod crypto;
pub mod deep_link;
pub mod encoding;
pub mod error;
pub mod i18n;
pub mod messages;
pub mod platform;
pub mod progress;
pub mod screens;
pub mod state;
pub mod task;
pub mod theme;

pub use app::*;
pub use archive::*;
pub use crypto::*;
pub use deep_link::*;
pub use encoding::*;
pub use error::*;
pub use i18n::*;
pub use messages::*;
pub use platform::*;
pub use progress::*;
pub use screens::*;
pub use state::*;
pub use theme::*;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(target_os = "android")]
pub(crate) mod android {
    use android_activity::AndroidApp;

    use crate::app::CryptonoteApp;
    use crate::deep_link::store_url;

    #[export_name = "android_main"]
    pub fn android_main(app: AndroidApp) {
        if let Some(url) = egui_shadcn::android::get_data_string(&app) {
            store_url(url);
        }
        egui_shadcn::android::run(app, "Cryptonote", |cc| {
            Ok(Box::new(CryptonoteApp::new(cc)) as Box<dyn eframe::App>)
        });
    }

    pub use egui_shadcn::android::poll_ime;
}
