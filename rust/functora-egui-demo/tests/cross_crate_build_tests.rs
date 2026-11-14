const SHARED_BUILD_SRC: &str = include_str!("../../functora-egui/src/build.rs");

#[test]
fn shared_codegen_generates_desktop_packaging() {
    for fragment in [
        "load_desktop_config",
        "LinuxDesktop",
        "Metainfo",
        "copy_desktop_icons",
        "desktop/linux",
        "desktop/metainfo",
    ] {
        assert!(
            SHARED_BUILD_SRC.contains(fragment),
            "shared codegen must produce desktop packaging, missing {fragment}"
        );
    }
}
