#![allow(clippy::unwrap_used, clippy::expect_used)]

const MAIN_SRC: &str = include_str!("../src/main.rs");
const DEEP_LINK_SRC: &str = include_str!("../src/deep_link.rs");
const APP_SRC: &str = include_str!("../src/app.rs");

#[test]
fn desktop_bin_uses_shared_runner() {
    for fragment in [
        "functora_egui::desktop::run",
        "CRYPTONOTE_DESKTOP_APP_ID",
        "CryptonoteApp::new",
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
fn desktop_file_args_feed_archive_store() {
    for fragment in [
        "ingest_desktop_args",
        "take_file_args",
        "store_archive",
        "ArchiveSource::Path",
        "cryptonote",
    ] {
        assert!(
            DEEP_LINK_SRC.contains(fragment),
            "desktop deep-link intake must map .cryptonote files, missing {fragment}"
        );
    }
}

#[test]
fn desktop_args_ingested_on_startup() {
    assert!(
        APP_SRC.contains("ingest_desktop_args"),
        "app startup must ingest desktop launch args"
    );
}
