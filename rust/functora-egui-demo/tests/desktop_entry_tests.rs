const MAIN_SRC: &str = include_str!("../src/main.rs");
const BUILD_SRC: &str = include_str!("../build.rs");
const MANIFEST: &str = include_str!("../Cargo.toml");

#[test]
fn desktop_bin_uses_shared_runner() {
    for fragment in [
        "functora_egui::desktop::run",
        "DEMO_DESKTOP_APP_ID",
        "ShowcaseApp::new",
    ] {
        assert!(
            MAIN_SRC.contains(fragment),
            "desktop bin must use the shared runner, missing {fragment}"
        );
    }
}

#[test]
fn desktop_bin_defines_no_android_entry() {
    assert!(
        !MAIN_SRC.contains("android_main"),
        "android entry must live in the shipped cdylib, not in the bin"
    );
}

#[test]
fn build_script_delegates_to_shared_codegen() {
    assert!(
        BUILD_SRC.contains("functora_egui::build::run"),
        "build script must call the shared codegen entry point"
    );
}

#[test]
fn manifest_declares_desktop_metadata() {
    assert!(
        MANIFEST.contains("[package.metadata.functora-egui-desktop]"),
        "manifest must declare desktop metadata"
    );
}
