//! egui-shadcn-demo: showcase app for the egui-shadcn widget library.

pub mod app;
pub mod sections;

pub use app::*;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(target_os = "android")]
pub(crate) mod android {
    use android_activity::AndroidApp;

    use crate::app::ShowcaseApp;

    #[export_name = "android_main"]
    pub fn android_main(app: AndroidApp) {
        egui_shadcn::android::run(app, "egui-shadcn Showcase", |cc| {
            Ok(Box::new(ShowcaseApp::new(cc)) as Box<dyn eframe::App>)
        });
    }

    pub use egui_shadcn::android::poll_ime;
}
