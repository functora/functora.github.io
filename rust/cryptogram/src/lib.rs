pub mod app;
pub mod error;
pub mod messages;
pub mod mls;
pub mod protocol;
pub mod relay;
pub mod route;
pub mod state;
pub mod vault;

#[cfg(target_arch = "wasm32")]
pub mod web;

pub use app::CryptogramApp;
pub use error::{Error, Result};
pub use route::Screen;
pub use state::AppState;

#[cfg(target_os = "android")]
pub(crate) mod android {
    use android_activity::AndroidApp;

    #[unsafe(export_name = "android_main")]
    pub fn android_main(app: AndroidApp) {
        functora_egui::android::run(app, "Cryptogram", |cc| {
            Ok(Box::new(crate::CryptogramApp::new(cc)) as Box<dyn eframe::App>)
        });
    }
}
