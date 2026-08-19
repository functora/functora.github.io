#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
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

#[cfg(any(target_arch = "wasm32", target_os = "android"))]
fn main() {}
