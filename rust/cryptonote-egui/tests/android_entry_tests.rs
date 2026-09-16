#![allow(clippy::unwrap_used, clippy::expect_used)]

const LIB_SRC: &str = include_str!("../src/lib.rs");
const MAIN_SRC: &str = include_str!("../src/main.rs");

#[test]
fn cdylib_exports_android_main_entry() {
    for fragment in [
        "export_name = \"android_main\"",
        "AndroidApp",
        "android::run",
        "CryptonoteApp::new",
    ] {
        assert!(
            LIB_SRC.contains(fragment),
            "cdylib root must define the android entry, missing {fragment}"
        );
    }
}

#[test]
fn unshipped_bin_defines_no_android_entry() {
    assert!(
        !MAIN_SRC.contains("android_main"),
        "android entry must live in the shipped cdylib, not in the bin"
    );
}
