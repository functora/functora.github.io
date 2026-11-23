use std::error::Error;
use std::path::Path;

const GENERATED_MODULE: &str = r#"    <script type="module">
      import { focusCanvas, registerServiceWorker } from './egui.js';
      import init from './pkg/cryptogram.js';
      await init();
      focusCanvas();
      registerServiceWorker('0.1.0');
    </script>"#;

fn main() -> Result<(), Box<dyn Error>> {
    let cfg = functora_egui::build::run("Cargo.toml", Path::new("../functora-egui"))?;
    println!(
        "cargo:rustc-env=CYPTOGRAM_DESKTOP_APP_ID={}",
        cfg.desktop.app_id
    );
    if std::env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("wasm32") {
        patch_web_assets()?;
    }
    Ok(())
}

fn patch_web_assets() -> Result<(), Box<dyn Error>> {
    println!("cargo:rerun-if-changed=assets/index.html");
    println!("cargo:rerun-if-changed=assets/cryptogram.js");
    println!("cargo:rerun-if-changed=assets/csp.txt");
    println!("cargo:rerun-if-changed=assets/telegram-web-app.js");
    let index_path = Path::new("assets/index.html");
    let index = std::fs::read_to_string(index_path)?;
    if !index.contains(GENERATED_MODULE) {
        return Err("generated web module marker was not found".into());
    }
    let csp_source = std::fs::read_to_string("assets/csp.txt")?;
    let csp_value = csp_source.trim();
    let head = format!(
        "    <meta http-equiv=\"Content-Security-Policy\" content=\"{csp_value}\" />\n    <script src=\"./telegram-web-app.js\"></script>\n"
    );
    let index_with_head = index.replacen("  </head>", &format!("{head}  </head>"), 1);
    let index_with_module = index_with_head.replace(
        GENERATED_MODULE,
        "    <script type=\"module\" src=\"./cryptogram.js\"></script>",
    );
    std::fs::write(index_path, index_with_module)?;
    Ok(())
}
