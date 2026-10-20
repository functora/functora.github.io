use std::path::Path;

fn main() -> Result<(), functora_egui::build::Error> {
    let cfg = functora_egui::build::run("Cargo.toml", Path::new("../functora-egui"))?;
    println!("cargo:rustc-env=DEMO_DESKTOP_APP_ID={}", cfg.desktop.app_id);
    println!("cargo:rustc-env=DEMO_WEB_TITLE={}", cfg.web.title);
    println!("cargo:rustc-env=DEMO_WEB_SHORT_NAME={}", cfg.web.short_name);
    println!(
        "cargo:rustc-env=DEMO_WEB_THEME_COLOR={}",
        cfg.web.theme_color
    );
    Ok(())
}
