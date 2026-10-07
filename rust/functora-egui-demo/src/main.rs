#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[cfg(not(any(target_arch = "wasm32", target_os = "android")))]
fn main() {
    functora_egui::desktop::run(
        "functora-egui Showcase",
        env!("DEMO_DESKTOP_APP_ID"),
        |cc| Ok(Box::new(functora_egui_demo::ShowcaseApp::new(cc)) as Box<dyn eframe::App>),
    );
}

#[cfg(any(target_arch = "wasm32", target_os = "android"))]
fn main() {}
