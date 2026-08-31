#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
fn main() -> eframe::Result {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1100.0, 750.0])
            .with_min_inner_size([320.0, 480.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Cryptonote",
        options,
        Box::new(|cc| Ok(Box::new(cryptonote_egui::CryptonoteApp::new(cc)))),
    )
}

#[cfg(any(target_arch = "wasm32", target_os = "android"))]
fn main() {}

#[cfg(target_os = "android")]
mod android {
    use android_activity::AndroidApp;

    #[unsafe(export_name = "android_main")]
    pub fn android_main(app: AndroidApp) {
        functora_egui::android::run(app, "Cryptonote", |cc| {
            Ok(Box::new(cryptonote_egui::CryptonoteApp::new(cc)) as Box<dyn eframe::App>)
        });
    }
}
