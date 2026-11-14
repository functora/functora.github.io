#![cfg(feature = "build")]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const CHILD_ENV: &str = "FUNCTORA_EGUI_CODEGEN_CHILD";
const MANIFEST_ENV: &str = "FUNCTORA_EGUI_CODEGEN_MANIFEST";

fn crate_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn read(path: &Path) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|err| panic!("read {}: {err}", path.display()))
}

fn run_codegen_child(cwd: &Path, manifest: &Path) -> Output {
    Command::new(std::env::current_exe().unwrap_or_else(|err| panic!("current exe: {err}")))
        .args(["--exact", "codegen_child", "--nocapture"])
        .env(CHILD_ENV, "1")
        .env(MANIFEST_ENV, manifest)
        .env("CARGO_CFG_TARGET_ARCH", "wasm32")
        .current_dir(cwd)
        .output()
        .unwrap_or_else(|err| panic!("run codegen child: {err}"))
}

#[test]
fn codegen_child() {
    if std::env::var(CHILD_ENV).is_err() {
        return;
    }
    let manifest =
        std::env::var(MANIFEST_ENV).unwrap_or_else(|err| panic!("codegen manifest env: {err}"));
    let configs = functora_egui::build::run(&manifest, Path::new("../missing-functora-egui"))
        .unwrap_or_else(|err| panic!("codegen failed: {err}"));
    assert_eq!(configs.web.pkg_js, "functora_egui.js");
}

#[test]
fn codegen_writes_web_assets_without_a_sibling_checkout() {
    let temp = tempfile::tempdir().unwrap_or_else(|err| panic!("temp dir: {err}"));
    let manifest = temp.path().join("Cargo.toml");
    let _copied = std::fs::copy(crate_root().join("Cargo.toml"), &manifest)
        .unwrap_or_else(|err| panic!("copy manifest: {err}"));
    let output = run_codegen_child(temp.path(), &manifest);
    assert!(
        output.status.success(),
        "codegen child failed without a sibling checkout: {}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        read(&temp.path().join("assets/egui.js")),
        read(&crate_root().join("assets/web/egui.js")),
        "generated web asset must match the bundled egui.js"
    );
    assert!(
        temp.path().join("assets/index.html").exists(),
        "codegen must write the web index"
    );
    assert!(
        temp.path().join("assets/manifest.webmanifest").exists(),
        "codegen must write the web manifest"
    );
}

#[test]
fn codegen_leaves_packaged_crate_sources_untouched() {
    let temp = tempfile::tempdir().unwrap_or_else(|err| panic!("temp dir: {err}"));
    let manifest = temp.path().join("Cargo.toml");
    let _copied = std::fs::copy(crate_root().join("Cargo.toml"), &manifest)
        .unwrap_or_else(|err| panic!("copy manifest: {err}"));
    std::fs::write(temp.path().join("Cargo.toml.orig"), "[package]\n")
        .unwrap_or_else(|err| panic!("write packaged marker: {err}"));
    let output = run_codegen_child(temp.path(), &manifest);
    assert!(
        output.status.success(),
        "codegen child failed for a packaged crate: {}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    for generated in ["android", "desktop", "assets"] {
        assert!(
            !temp.path().join(generated).exists(),
            "codegen must not write {generated}/ into a packaged crate"
        );
    }
    assert!(
        !temp.path().join(".gitignore").exists(),
        "codegen must not rewrite .gitignore inside a packaged crate"
    );
}
