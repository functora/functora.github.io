//! functora-egui-demo: showcase app for the functora-egui widget library.

pub mod app;
pub mod route;
pub mod sections;

pub use app::*;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(target_os = "android")]
pub(crate) mod android {
    use android_activity::AndroidApp;

    use crate::app::ShowcaseApp;

    #[unsafe(export_name = "android_main")]
    pub fn android_main(app: AndroidApp) {
        functora_egui::android::run(app, "functora-egui Showcase", |cc| {
            Ok(Box::new(ShowcaseApp::new(cc)) as Box<dyn eframe::App>)
        });
    }

    pub use functora_egui::android::poll_ime;
}
