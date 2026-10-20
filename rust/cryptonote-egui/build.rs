use std::path::Path;

fn main() -> Result<(), functora_egui::build::Error> {
    let cfg = functora_egui::build::run("Cargo.toml", Path::new("../functora-egui"))?;
    println!("cargo:rustc-env=CRYPTONOTE_DESKTOP_APP_ID={}", cfg.desktop.app_id);
    Ok(())
}
