#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[cfg(not(target_arch = "wasm32"))]
fn main() -> eframe::Result {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([420.0, 720.0])
            .with_min_inner_size([320.0, 480.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Cryptonote",
        options,
        Box::new(|cc| Ok(Box::new(cryptonote_egui::CryptonoteApp::new(cc)))),
    )
}

#[cfg(target_arch = "wasm32")]
fn main() {}

#[cfg(target_os = "android")]
#[export_name = "android_main"]
pub fn android_main(app: winit::platform::android::activity::AndroidApp) {
    let options = eframe::NativeOptions {
        android_app: Some(app),
        viewport: egui::ViewportBuilder::default(),
        ..Default::default()
    };
    let result = eframe::run_native(
        "Cryptonote",
        options,
        Box::new(|cc| Ok(Box::new(cryptonote_egui::CryptonoteApp::new(cc)))),
    );
    if let Err(error) = result {
        eprintln!("eframe error: {error}");
    }
}
