#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
fn main() {
    functora_egui::desktop::run("Cryptonote", env!("CRYPTONOTE_DESKTOP_APP_ID"), |cc| {
        Ok(Box::new(cryptonote_egui::CryptonoteApp::new(cc)) as Box<dyn eframe::App>)
    });
}

#[cfg(any(target_arch = "wasm32", target_os = "android"))]
fn main() {}
